{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Lens hiding (Context)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Data.Tree
import           Data.Graph.Inductive.Graph
import           Data.Graph.Inductive.Query.DFS (xdffWith)
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

type Handler = StateT DB IO

data DB = DB
        { _gr :: Gr'
        , _nodeEnum :: Node
        , _rootNode :: Node -- ^ Parent of everything
        , _viewContext :: Node -- ^ Active context root
        } deriving (Show, Read)

type Gr' = Gr Thingy Relation

-- | Nodes
data Thingy = Thingy
    { _created :: UTCTime
    , _content :: String
    , _flags :: Map Flag String
    } deriving (Show, Read, Eq)

data Flag = Done deriving (Show, Read, Eq, Ord)

-- | Edges
type Relation = String

makeLenses ''Thingy
makeLenses ''DB

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
setFlagGr n f v = use gr >>= go
  where
    go :: Gr' -> Handler ()
    go g | Just thingy <- lab g n = gr .= insNode (n, flags %~ Map.insert f v $ thingy) g
         | otherwise              = liftIO $ putStrLn "Node not found"

updateContentGr :: Node -> String -> Handler ()
updateContentGr n c = use gr >>= go
  where
    go :: Gr' -> Handler ()
    go g | Just thingy <- lab g n = gr .= insNode (n, content .~ c $ thingy) g
         | otherwise              = nodeNotFound

-- * Viewing

type View = Tree (Context Thingy Relation)

-- -- | XXX: Doesn't detect cycles!
-- viewSuc :: Gr' -> Node -> View
-- viewSuc gr nd = View node thingy (each._2 %~ viewSuc gr $ children)
--   where
--     (_, node, thingy, children) = context gr nd

-- * Render

data Renderer = Renderer
              { renderViewRoot :: View -> P.Doc
              , renderViewSub :: View -> P.Doc
              , renderViewForest :: Forest (Context Thingy Relation) -> P.Doc
              , renderThingyLine :: Node -> Thingy -> P.Doc
              , renderRelation :: Relation -> P.Doc
              , renderFlag :: Flag -> P.Doc
              }

defaultRenderer :: Renderer
defaultRenderer = Renderer
    { renderViewRoot = \(Node (_,node,thingy,_) sub) -> P.hang 2 $
        P.dullblue (renderThingyLine defaultRenderer node thingy) P.<$$>
        (renderViewForest defaultRenderer sub)

    , renderViewSub = \(Node (_,node,thingy,_) sub) -> P.hang 2 $
        if null sub
            then renderThingyLine defaultRenderer node thingy
            else renderThingyLine defaultRenderer node thingy P.<$$> renderViewForest defaultRenderer sub

    , renderViewForest = P.vcat . map (\view ->
        {- renderRelation defaultRenderer rel P.<+> -} (renderViewSub defaultRenderer view))

    , renderThingyLine = \node Thingy{..} ->
        P.text _content
        P.<+>
        P.yellow (P.brackets $ P.dullmagenta $ P.text (show node))
        P.<+>
        (if Map.null _flags then P.empty else P.tupled $ map (renderFlag defaultRenderer) $ Map.keys _flags)

    , renderRelation = \case
        "child" -> P.blue "*"
        other   -> P.red $ P.text other

    , renderFlag = \case
        Done -> P.dullgreen "done"
    }

data DocFx = DocFx P.Doc [DocFx]

printTreeAt :: Node -> Handler ()
printTreeAt node = do
    g <- use gr
    let recurse (_,_,_,xs) = map snd $ filter (shouldShow g) xs

    forest <- xdffWith recurse id [node] <$> use gr
    liftIO $ mapM_ (P.putDoc . renderViewRoot defaultRenderer) forest

shouldShow :: Gr' -> (Relation, Node) -> Bool
shouldShow g (rel, nd) = maybe False (Map.notMember Done . _flags) (lab g nd)

-- | Print the (direct or indirect) children of a node.
printChildren :: Node -> Handler ()
printChildren nd = do
    DB{..} <- get
    -- liftIO $ P.putDoc $ renderViewRoot defaultRenderer (viewSuc _gr nd)
    -- liftIO $ P.putDoc $ foldingDoc _gr nd
    printTreeAt nd
    liftIO $ putStrLn ""

printNode :: Node -> Handler ()
printNode node = do
    mthingy <- use gr <&> flip lab node
    maybe nodeNotFound (liftIO . P.putDoc . renderThingyLine defaultRenderer node) mthingy 

-- * Interactive CLI

commands :: Commands Handler
commands = Node
    (command "ggtd" "A graphy getting-things-done application" . io $ lift $ putStrLn "No command given; try \"ggtd help\".")
    [
      Node (command "ls" "List thingies" lsAction) []
    , Node (command "context" "Set the active context to the given node" setContextAction) []
    , Node (command "node" "Add, modify and delete nodes" nodeAction)
        [ Node (command "child" "Create a new child with parent set to current context" addThingyAction) []
        , Node (command "new" "Create a new node with the specified edge" createAction) [] 
        , Node (command "done" "Set the task done" doneAction) []
        , Node (command "set" "Update the content" updateAction) []
        ]
    , Node (command "quit" "Quit the application" $ io $ liftIO exitSuccess) []
    , Node (command "help" "Show help" $ io $ showUsage commands) []
    ]

addThingyAction :: Action Handler
addThingyAction = withOption contentOpt $ \cnt -> io $ do
    thingy <- createThingy cnt
    node <- addThingyGr thingy
    parent <- use viewContext
    addRelGr (parent, node, "child")
    saveDB
    printChildren =<< use viewContext

lsAction :: Action Handler
lsAction = io $ do
    node <- use viewContext
    printChildren node

setContextAction :: Action Handler
setContextAction = withNonOption (Arg.optional (-1) nodeType) $ \node -> io $ do
    current <- use viewContext
    let onSuccess            = viewContext .= node >> printChildren node
        onError | node == -1 = liftIO $ putStrLn $ "Current context is " ++ show current
                | otherwise  = nodeNotFound
    ifExists node onError onSuccess

doneAction :: Action Handler
doneAction = withNonOption nodeType $ \node -> io $ do
    setFlagGr node Done ""
    saveDB

updateAction :: Action Handler
updateAction = withNonOption nodeType $ \node -> withNonOption Arg.string $ \cnt -> io $ do
    updateContentGr node cnt
    saveDB

-- | The default action is to create a node with the given content.
nodeAction :: Action Handler
nodeAction = withNonOption Arg.string $ \cnt -> io $ do
    thingy <- createThingy cnt
    node <- addThingyGr thingy
    parent <- use viewContext
    addRelGr (parent, node, "child")
    saveDB
    printChildren =<< use viewContext

createAction :: Action Handler
createAction = withOption relOpt $ \rel -> withOption contentOpt $ \cnt -> io $ do
    thingy <- createThingy cnt
    node <- addThingyGr thingy
    parent <- use viewContext
    addRelGr (parent, node, rel)
    printNode node
    saveDB

-- ** Utility

nodeOpt :: Option Int
nodeOpt = option "n" ["node"] nodeType (-1) "Node identifier"

nodeType :: Arg.Type Node
nodeType = fromIntegral <$> Arg.integer

contentOpt :: Option String
contentOpt = option "c" ["content"] Arg.string
    { parser = \str -> if null str then Left "Cannot be empty" else Right str }
    "" "The content"

relOpt :: Option Relation
relOpt = option "r" ["relation"] Arg.string
    { parser = \str -> if null str then Left "Relation cannot be empty" else Right str }
    "child" "Relation to context"

nodeNotFound :: Handler ()
nodeNotFound = liftIO $ putStrLn "Node not found"

ifExists :: Node -> Handler () -> Handler () -> Handler ()
ifExists node failure success = do
    exists <- use gr <&> gelem node
    if exists then success else failure
