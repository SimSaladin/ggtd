------------------------------------------------------------------------------
-- |
-- Module         : GGTD.Relation
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
--
-- We call the edges of the graph relations. Relations basically define the
-- structure of the graph and with different relations (different labels on
-- the edges) we can define kind-of subgraphs, like groups with "relGroup",
-- contexts with "relLink". The most common relation is "relChild",
------------------------------------------------------------------------------
module GGTD.Relation where

import GGTD.Base

-- | In a kind of main hierarchial view, where root (node 0) is at the top,
-- there are groups at the middle and "relChild"'s at the leaves.
relGroup :: Relation
relGroup = "group"

-- | Single item that could be actionable; i.e. it may have a waiting- or
-- done-flag set, but if it didn't it would qualify as something to do.
relChild :: Relation
relChild = "child"

-- | A reference is some kind of resource that is relevant to what links to
-- it.
relRef :: Relation
relRef = "ref"

-- | A link is somewhat like a 'relGroup', with the exception that the
-- successors of a node that is linked to will not be considered by any
-- traversal. (The direct target of a link edge itself is.) Links are a way
-- to make different projections of the graph, for example to separate
-- contexts for at home, at work etc.
relLink :: Relation
relLink = "link"

-- | List of all relations we use.
relations :: [Relation]
relations = [relGroup, relChild, relLink, relRef]
