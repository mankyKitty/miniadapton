{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Lib where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (StateT, MonadState (..))
import Control.Monad.Except (ExceptT, MonadError (..))

import Data.Kind (Type)
import Data.IntMap (IntMap)
import Data.Set (Set)

import qualified Data.IORef as IORef

data NodeVal m a
  = ARef (m a)
  | AThunk (a)

-- Edges are labeled with the value returned by that call.
data Edge m a = Edge
  { _edge_dirty :: Bool
  , _edge_source :: Node m
  , _edge_target :: Node m
  , _edge_label :: m (Maybe a)
  }

-- We maintain edges bidirectionally:
-- Each node stores:
-- - An ordered list of outgoing edges that is appended by each call to get or force
-- - An unordered set of incoming edges.
-- This allows us to traverse the DCG from caller to callee or vice-versa.
data Node m = forall a. Node
  { _node_value :: NodeVal m a
  , _node_outgoinglist :: forall x. IntMap (Edge m x)
  , _node_incomingset :: forall x. [Edge m x]
  }

-- The DCG is initially empty at the beginning of the execution.
-- Nodes are added to the DCG whenever a new aref or athunk is created via:
-- - aref
-- - athunk
-- - or a memo constructor (when memoization misses).
--
-- Edges are:
-- - added when an athunk calls get or force;
-- - labeled with the value returned by that call.
data DCG m = DCG
  { _dcg_nodes :: [Node m]
  , _dcg_edges :: forall x. [Edge m x]
  }

-- The dirtying phase occurs when we make calls to set to update inputs
-- to the incremental program. For each call to 'set', we traverse the
-- DCG starting from the 'aref' backward, marking all traversed edges as “dirty”
--
-- 1 function dirty(node)
-- 2   foreach edge ∈ node.incomingset do
-- 3     if ¬edge.dirty then
-- 4       edge.dirty ← true;
-- 5       dirty(edge.source);

-- The propagation phase occurs when we make calls to 'force' to demand results
-- from the incremental program. For each call to force on an 'athunk', we perform
-- an inorder traversal starting from the athunk’s dirty outgoing edges,
-- re-evaluating athunks as necessary.
--
--  6 function propagate(node)
--
--       # We check if we need to re-evaluate an athunk by iterating over its dirty outgoing edges in the order they were added (line 7)
--  7   foreach edge ∈ node.outgoinglist do
--  8     if edge.dirty then
--
--          # For eachdirty edge, we first clean its dirty flag (line 9).
--  9       edge.dirty ← false;
--
--          # If the target node is an athunk, we recursively check if we need to re-evaluate the target athunk (lines 10 to 11).
-- 10       if edge.target is athunk then
-- 11         propagate(edge.target);
--
--          # Then, we compare the value of the target aref or athunk against the label of the outgoing edge (line 12).
-- 12       if edge.label =/= edge.target.value then
--
--            # If the value is inconsistent, we know that at least one input to the athunk has changed, so
--            # we need to re-evaluate the athunk (line 14), and need not check its remaining outgoing edges (line 15).
--
--            # In fact, we first remove all its outgoing edges (line 13), since some edges may no longer be relevant
--            # due to the changed input; relevant edges will be re-added when get or force is called during re-evaluation.
-- 13         node.outgoinglist ← [];
-- 14         evaluate(node);
-- 15         return;

-- type 'a aref
-- val aref : 'a → 'a aref
-- val get : 'a aref → 'a
-- val set : 'a aref → 'a → unit
newtype M k m a = M
  { unM :: m (k a)
  }

-- aref :: a -> M m a
-- aref = undefined

-- get :: M m a -> m (IORef.IORef a)
-- get = undefined

-- set :: M m a -> a -> m ()
-- set = undefined

-- type 'a athunk
-- val force : 'a athunk → 'a
-- val thunk : (unit → 'a) → 'a athunk
-- val memo : ('fn → 'arg → 'a) → (('arg → 'a athunk) as 'fn)
-- data U a

-- force :: U a -> a
-- force = undefined

-- thunk :: (() -> a) -> U a
-- thunk = undefined

-- memo :: (arg -> a) -> arg -> U a
-- memo = undefined

newtype U m a = U { runU :: () -> m a }

class CanMimidapton (m :: Type -> Type) where
  type RefContainer m:: Type -> Type

  aref :: a -> M (RefContainer m) m a
  get :: M (RefContainer m) m a -> m (RefContainer m a)
  set :: M (RefContainer m) m a -> a -> m ()

  -- Thunk API
  force :: U m a -> m a
  thunk :: (() -> m a) -> U m a
  memo :: (arg -> m a) -> arg -> U m a

data MimiErr

newtype MimiM m a = MimiM
  { runMimiM :: StateT (DCG m) (ExceptT MimiErr m) a
  }
  deriving (
    Functor,
    Applicative,
    Monad,
    MonadIO,
    MonadState (DCG m),
    MonadError MimiErr
  )

instance MonadIO m => CanMimidapton (MimiM m) where
  type RefContainer (MimiM m) = IORef.IORef

  aref = M . liftIO . IORef.newIORef
  get = unM
  set (M refA) a = refA >>= liftIO . flip IORef.writeIORef a

  force (U t) = t ()
  thunk = U
  memo f a = U (\_ -> f a)