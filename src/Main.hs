{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Lens hiding ((&), Context, Context')
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Data.Function (on)
import           Data.Tree
import           Data.Char
import           Data.Ord (comparing, Down(..))
import           Data.List (sortBy)
import           Data.Tuple (swap)
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.Query.DFS (CFun)
import           Data.Graph.Inductive.PatriciaTree
import           Data.Time
import           Data.Map (Map)
import qualified Data.Map as Map
import           System.Console.Argument (Option, option, Type(..))
import qualified System.Console.Argument as Arg
import           System.Console.Command
import           System.Console.Program (interactive, showUsage)
import           System.Directory (doesFileExist)
import           System.Exit (exitSuccess)
import qualified Text.PrettyPrint.ANSI.Leijen as P
import qualified Text.Regex as Regex
import qualified Text.Regex.Base as Regex

type Handler = StateT DB IO

data DB = DB
        { _gr :: Gr'
        , _nodeEnum :: Node
        , _rootNode :: Node -- ^ Parent of everything
        , _viewContext :: Node -- ^ Active context root
        } deriving (Show, Read)

type Gr' = Gr Thingy Relation
type Context' = Context Thingy Relation

-- | Nodes
data Thingy = Thingy
    { _created :: UTCTime
    , _content :: String
    , _flags :: Map Flag String
    } deriving (Show, Read, Eq)

data Flag = Done
          | Wait
          | Priority
          deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | Edges
type Relation = String

makeLenses ''Thingy
makeLenses ''DB

data Filter = FFlag Flag
            | FNotFlag Flag
            | FNotContent String
            | FNone

-- | Sorting stuff.
data Sort = SFlag Flag
          | SCreated
          | SRelation (Relation -> Relation -> Ordering)
          | SDesc Sort
          | SNone

-- | For this autogeneration to be sensible, every Flag constructor must
-- have unique first character.
filters :: [Arg.Option Filter]
filters = do
    flg <- [minBound..maxBound]

    let parse :: String -> Either String Filter
        parse "no"  = Right (FNotFlag flg)
        parse "yes" = Right (FFlag flg)
        parse ""    = Right (FFlag flg)
        parse x     = Left $ "Expected yes, no or nothing but got: \"" ++ x ++ "\" instead."

        short = toLower $ head (show flg)
        long  = map toLower $ show flg
        typ   = Arg.Type parse "NO" Nothing
        desc  = "Filter by flag \"" ++ show flg ++ "\""

    return $ option [short] [long] typ (flagFilterDefault flg) desc

-- | Set default filter options here.
flagFilterDefault :: Flag -> Filter
flagFilterDefault Done = FNotFlag Done
flagFilterDefault _    = FNone

-- | True if passes all filters.
applyFilters :: [Filter] -> (Relation, Context') -> Bool
applyFilters fltrs (_, (_,_,thingy,_)) = all applyFilter fltrs
  where
    applyFilter FNone = True
    applyFilter (FNotFlag flg) = Map.notMember flg (_flags thingy)
    applyFilter (FFlag flg) = Map.member flg (_flags thingy)
    applyFilter (FNotContent str) = _content thingy /= str

flagSortDefault :: Flag -> Sort
flagSortDefault Wait = SDesc (SFlag Wait)
flagSortDefault Priority = SFlag Priority
flagSortDefault _ = SNone

-- | Note that the order is important: sorts deciding at the head of the
-- list always takes precedence.
sorts :: [Arg.Option Sort]
sorts = flagSorts ++ [createdSort]
  where
    createdSort =
        let parse :: String -> Either String Sort
            parse "desc" = Right $ SDesc SCreated
            parse "asc"  = Right SCreated
            parse ""     = Right SCreated
            parse x      = Left $ "Expected desc, asc or nothing but got: \"" ++ x ++ "\" instead."

            in option "c" ["created"] (Arg.Type parse "ASC" Nothing) (SDesc SCreated) "Sort by creation date"

    flagSorts = do
        flg <- [minBound..maxBound]

        let parse :: String -> Either String Sort
            parse "desc" = Right (SDesc $ SFlag flg)
            parse "asc"  = Right (SFlag flg)
            parse ""     = Right (SDesc $ SFlag flg)
            parse x      = Left $ "Expected desc, asc or nothing but got: \"" ++ x ++ "\" instead."

            short = head (show flg)
            long  = "sort-by-" ++ map toLower (show flg)
            typ   = Arg.Type parse "ASC" Nothing
            desc  = "Sort by flag \"" ++ show flg ++ "\""

        return $ option [short] [long] typ (flagSortDefault flg) desc

applySorts :: [Sort] -> [(Relation, Context')] -> [(Relation, Context')]
applySorts srt = sortBy (buildSortOrd srt)
  where
    buildSortOrd :: [Sort] -> (Relation, Context') -> (Relation, Context') -> Ordering
    buildSortOrd []     = \_ _ -> EQ
    buildSortOrd (x:xs) = sortFunc x `combining` buildSortOrd xs
        
    sortFunc :: Sort -> (Relation, Context') -> (Relation, Context') -> Ordering
    sortFunc x = case x of
        SFlag flag   -> comparing $ Down . viewFlag flag
        SCreated     -> comparing $ _created . lab' . snd
        SRelation f  -> f `on` fst
        SDesc s      -> \a b -> case sortFunc s a b of
                                    LT -> GT
                                    GT -> LT
                                    EQ -> EQ
        SNone        -> \_ _ -> EQ

    viewFlag flag (_, ctx) = Map.lookup flag . _flags $ lab' ctx

    combining :: (a -> a -> Ordering) -> (a -> a -> Ordering) -> a -> a -> Ordering
    combining f g x y = case (f x y, g x y) of
                            (EQ, r) -> r
                            (r, _)  -> r

sortFirstRel :: Relation -> Sort
sortFirstRel rel = SRelation $ \a b -> if | a == rel && b == rel -> EQ
                                          | a == rel             -> LT
                                          | b == rel             -> GT
                                          | otherwise            -> EQ

-- * Main

main :: IO ()
main = do
    db <- loadDB
    void $ runStateT (interactive commands) db

-- | Read the DB from ./ggtd.db
loadDB :: IO DB
loadDB = do
    exists <- doesFileExist "ggtd.db"
    if exists
        then do db <- read <$> readFile "ggtd.db"
                _gr db `seq` return db -- TODO: should use acid-state?
        else do root <- createThingy "All"
                return $ DB
                    { _gr = insNode (0, root) empty
                    , _rootNode = 0
                    , _nodeEnum = 1
                    , _viewContext = 0
                    }

-- | Save the DB to disk.
saveDB :: Handler ()
saveDB = do
    db <- get
    lift $ writeFile "ggtd.db" (show db)

-- * Query

type View = Tree (Relation, Context') -- ^ The context, and via which path we reached it

getViewAtGr :: [Filter] -> [Sort] -> Node -> Handler ([View], Gr')
getViewAtGr fltr srt node = do
    g <- use gr
    let recurse :: Context' -> [(Node, Relation)]
        recurse (_,_,_,xs) = map swap . (each._2 %~ node') . applySorts srt . filter (applyFilters fltr) $ (each._2 %~ context g) xs
    xdfWith' recurse (,) [(node, "")] <$> use gr

-- * Modify

createThingy :: MonadIO m => String -> m Thingy
createThingy txt = Thingy <$> liftIO getCurrentTime <*> pure txt <*> pure Map.empty

-- | Creates the new node and adds correct edges.
addThingyGr :: Thingy -> Handler Node
addThingyGr thingy = do
    nxt <- nodeEnum <+= 1
    gr %= insNode (nxt, thingy)
    return nxt

addRelGr :: (Node, Node, Relation) -> Handler ()
addRelGr edge = gr %= insEdge edge

setFlagGr :: Node -> Flag -> String -> Handler ()
setFlagGr n f v = overNode n (_3.flags %~ Map.insert f v)

updateContentGr :: Node -> String -> Handler ()
updateContentGr node cnt = overNode node (_3.content .~ cnt)

-- | Set all relations to the node to be of the given type.
setRelGr :: Node -> Relation -> Handler ()
setRelGr node rel = overNode node (_1.each._1 .~ rel)

setParentGr :: Node -> Node -> Handler ()
setParentGr node parent = overNode node (_1.each._2 .~ parent)

-- | Combinator used by graph modifying code.
overNode :: Node -> (Context' -> Context') -> Handler ()
overNode node f = use gr >>= go
  where
    go g | (Just ctx, g') <- match node g = gr .= (f ctx & g')
         | otherwise                      = nodeNotFound

-- * Render

data Renderer = Renderer
              { renderViewRoot :: View -> P.Doc
              , renderViewForest :: [View] -> P.Doc
              , renderThingyLine :: Node -> Thingy -> P.Doc
              , renderFlag :: Flag -> P.Doc
              , renderContent :: String -> P.Doc
              }

relGroup, relChild :: Relation
relGroup = "group"
relChild = "child"

relations :: [Relation]
relations = [relGroup, relChild]

defaultRenderer :: Renderer
defaultRenderer = Renderer
    { renderViewRoot = \(Node (_, ctx) sub) ->
        P.hang 2 $ P.dullblue (renderThingyLine defaultRenderer (node' ctx) (lab' ctx))
            P.<$$> renderViewForest defaultRenderer sub

    , renderViewForest = P.vcat . map (\(Node (rel, ctx) sub) ->
        let header = if | rel == relGroup -> P.magenta
                        | rel == relChild -> (P.red "*" P.<+>)
                        | otherwise       -> (P.blue (P.text rel) P.<+>)

        in P.hang 2 $ if null sub
                then header (renderThingyLine defaultRenderer (node' ctx) (lab' ctx))
                else header (renderThingyLine defaultRenderer (node' ctx) (lab' ctx))
                        P.<$$> renderViewForest defaultRenderer sub
        )

    , renderThingyLine = \node Thingy{..} ->
        renderContent defaultRenderer _content
        P.<+>
        P.yellow (P.brackets $ P.dullmagenta $ P.text (show node))
        P.<+>
        (if Map.null _flags then P.empty else P.tupled $ map (renderFlag defaultRenderer) $ Map.keys _flags)

    , renderContent = \cnt ->
        let rticket = Regex.makeRegex ("(#[0-9]*)" :: String) :: Regex.Regex
        in case Regex.getAllSubmatches $ Regex.match rticket cnt of
            [_,(start,len)]
                | (s1, s2) <- splitAt start cnt -> P.text s1 P.<> P.dullcyan (P.text (take len s2)) P.<> P.text (drop len s2)
            _ -> P.text cnt

    , renderFlag = \case
        Done -> P.dullgreen "done"
        Wait -> P.dullyellow "waiting"
        Priority -> P.red "PRIORITY"
    }

data DocFx = DocFx P.Doc [DocFx]

printTreeAt :: [Filter] -> [Sort] -> Node -> Handler ()
printTreeAt fltr srt node = liftIO . mapM_ (P.putDoc . renderViewRoot defaultRenderer) . fst =<< getViewAtGr fltr srt node

-- | Print the (direct or indirect) children of a node.
printChildren :: [Filter] -> [Sort] -> Node -> Handler ()
printChildren fltrs srts nd = do
    DB{..} <- get
    printTreeAt fltrs srts nd
    liftIO $ putStrLn ""

printChildrenFlat :: [Filter] -> [Sort] -> Node -> Handler ()
printChildrenFlat fltr srt node = do
    (forest, _) <- getViewAtGr fltr srt node
    liftIO $ mapM_ (\(_, ctx) -> P.putDoc $ renderThingyLine defaultRenderer (node' ctx) (lab' ctx) P.<> P.hardline) $ filter isActionable $ concatMap flatten forest

-- | Only @relChild@'s are actionable
isActionable :: (Relation, Context') -> Bool
isActionable (rel, _) = rel == relChild

printNode :: Node -> Handler ()
printNode node = do
    mthingy <- use gr <&> flip lab node
    maybe nodeNotFound (liftIO . P.putDoc . renderThingyLine defaultRenderer node) mthingy 

-- * Interactive CLI

commands :: Commands Handler
commands = Node
    (command "ggtd" "A graphy getting-things-done application" . io $ lift $ putStrLn "No command given; try \"ggtd help\".")
    [
      Node (command "ls" "List thingies. By default lists all not-done tasks starting at current view context." lsAction) []
    , Node (command "todo" "Print a flat list of items, a TODO list" todoAction) []
    , Node (command "context" "Set the active context to the given node" setContextAction) []
    , Node (command "node" "Add, modify and delete nodes. node \"Some node description\" creates a node with the given description under the current view context." nodeAction)
        [
          Node (command "create" "Create a new node with the specified edge" nodeCreateAction) [] 
        , Node (command "update" "Update the content" nodeUpdateAction) []
        , Node (command "done" "Set the task done" $ nodeFlagAction Done) []
        , Node (command "wait" "Set to waiting state" $ nodeFlagAction Wait) []
        , Node (command "priority" "Set node priority" $ nodePriorityAction) []
        ]
    , Node (command "edge" "Add, modify or delete edges" edgeAction)
        [ 
          Node (command "relation" "Change the relation type" edgeChangeAction) []
        , Node (command "parent" "Change the parent of a node" edgeParentAction) []
        ]
    , Node (command "quit" "Quit the application" $ io $ liftIO exitSuccess) []
    , Node (command "help" "Show help" $ io $ showUsage commands) []
    ]

lsAction :: Action Handler
lsAction = foldingOpts filters $ \fltr -> foldingOpts sorts $ \srt -> io $ do
    node <- use viewContext
    printChildren fltr srt node

todoAction :: Action Handler
todoAction = foldingOpts filters $ \flt_opt -> foldingOpts sorts $ \srt_opt -> io $ do
    let flt = FNotContent "someday/maybe" : flt_opt
        srt = sortFirstRel "child" : srt_opt
    printChildrenFlat flt srt 0

setContextAction :: Action Handler
setContextAction = withNonOption (Arg.optional (-1) nodeType) $ \node -> io $ do
    current <- use viewContext
    let onSuccess            = viewContext .= node >> printChildren [] [] node
        onError | node == -1 = liftIO $ putStrLn $ "Current context is " ++ show current
                | otherwise  = nodeNotFound
    ifExists node onError onSuccess

-- ** Nodes

-- | The default action is to create a node with the given content.
nodeAction :: Action Handler
nodeAction = withNonOption contentType $ \cnt -> io $ do
    thingy <- createThingy cnt
    node <- addThingyGr thingy
    parent <- use viewContext
    addRelGr (parent, node, "child")
    saveDB
    printChildren [] [] =<< use viewContext

nodeCreateAction :: Action Handler
nodeCreateAction =
    withOption parentOpt $ \nd ->
    withNonOption relType $ \rel ->
    withNonOption contentType $ \cnt ->
    io $ do
        thingy <- createThingy cnt
        node <- addThingyGr thingy
        parent <- if nd >= 0 then return nd else use viewContext
        addRelGr (parent, node, rel)
        printNode node
        saveDB

nodeUpdateAction :: Action Handler
nodeUpdateAction = withNonOption nodeType $ \node -> withNonOption Arg.string $ \cnt -> io $ do
    updateContentGr node cnt
    saveDB

nodeFlagAction :: Flag -> Action Handler
nodeFlagAction flag = withNonOption nodeType $ \node -> io $ do
    setFlagGr node flag ""
    saveDB

nodePriorityAction :: Action Handler
nodePriorityAction = withNonOption nodeType $ \node ->
    withNonOption Arg.integer $ \int ->
    io $ do
        setFlagGr node Priority (show int)
        saveDB

-- ** Edges

edgeAction :: Action Handler
edgeAction = io $ do
    lift $ putStrLn "Not yet implemented"

edgeChangeAction :: Action Handler
edgeChangeAction = withNonOption nodeType $ \node -> withNonOption relType $ \rel -> io $ do
    setRelGr node rel
    printNode node
    saveDB

edgeParentAction :: Action Handler
edgeParentAction = withNonOption nodeType $ \node -> withNonOption nodeType $ \newParent -> io $ do
    setParentGr node newParent
    printNode node
    saveDB

-- * Arguments

nodeOpt :: Option Node
nodeOpt = option "n" ["node"] nodeType (-1) "Node"

parentOpt :: Option Node
parentOpt = option "p" ["parent"] nodeType (-1) "Parent node"

contentOpt :: Option String
contentOpt = option "c" ["content"] Arg.string
    { parser = \str -> if null str then Left "Cannot be empty" else Right str }
    "" "The content"

relOpt :: Option Relation
relOpt = option "r" ["relation"] Arg.string
    { parser = \str -> if null str then Left "Relation cannot be empty" else Right str }
    "child" "Relation to context"

-- ** Types

nodeType :: Arg.Type Node
nodeType = fromIntegral <$> Arg.integer { Arg.name = "NODE" }

relType :: Arg.Type Relation
relType = Arg.string { Arg.name = "RELATION" }

contentType :: Arg.Type String
contentType = Arg.string { Arg.name = "CONTENT" }

-- * Utility

nodeNotFound :: Handler ()
nodeNotFound = liftIO $ putStrLn "Node not found"

ifExists :: Node -> Handler () -> Handler () -> Handler ()
ifExists node failure success = do
    exists <- use gr <&> gelem node
    if exists then success else failure

foldingOpts :: MonadIO m => [Option a] -> ([a] -> Action m) -> Action m
foldingOpts (o:os) f = withOption o $ \r -> foldingOpts os (\rs -> f (r:rs))
foldingOpts [] f = f []

-- * Graph algorithms

-- | like @xdfWith@, but with extra information about the traversed nodes
-- propagated into the tree.
xdfWith' :: (Graph gr)
    => CFun a b [(Node, e)]
    -> (e -> CFun a b c)
    -> [(Node, e)]
    -> gr a b
    -> ([Tree c],gr a b)
xdfWith' _ _ []     g             = ([],g)
xdfWith' _ _ _      g | isEmpty g = ([],g)
xdfWith' d f ((v,e):vs) g = case match v g of
                        (Nothing,g1) -> xdfWith' d f vs g1
                        (Just c,g1)  -> (Node (f e c) ts:ts',g3)
                                 where (ts,g2)  = xdfWith' d f (d c) g1
                                       (ts',g3) = xdfWith' d f vs g2
