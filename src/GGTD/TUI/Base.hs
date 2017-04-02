------------------------------------------------------------------------------
-- |
-- Module         : GGTD.TUI.Base
-- Copyright      : (C) 2016 Samuli Thomasson
-- License        : %% (see the file LICENSE)
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module GGTD.TUI.Base
    ( module X
    , AppFocus(..)
    , MyEvent(..), emit, evchan
    ) where

import           Control.Monad              as X
import           Control.Lens               as X hiding ((&), Context, Context', pre)
import           Control.Monad.IO.Class     as X (liftIO)

import           Data.Text                  as X (Text, pack, unpack)
import           Data.Monoid                as X ((<>))
import           Data.Default               as X
import           Data.Graph.Inductive.Graph as X

import           Brick                      as X hiding (Context)
import           Graphics.Vty               as X hiding (showCursor)

import           GGTD.Base                  as X
import           GGTD.Relation              as X
import           GGTD.Filter                as X
import           GGTD.Sort                  as X
import           GGTD.DB                    as X (runHandler)

import           Brick.BChan
import           System.IO.Unsafe (unsafePerformIO)

data AppFocus = FocusTreeNav
              | FocusTaskList
              | FocusLineInput

data MyEvent = SetStatus Text -- ^ Set status line content
             | UIError Text -- ^ User error
             | SetFocus AppFocus
             | RefreshTreeNav
             | RefreshTaskList

evchan :: BChan MyEvent
evchan = unsafePerformIO (newBChan 100)
{-# NOINLINE evchan #-}

emit :: MyEvent -> Handler ()
emit = liftIO . writeBChan evchan
