-- | 'MSF's with a 'State' monadic layer.
--
-- This module contains functions to work with 'MSF's that include a 'State'
-- monadic layer. This includes functions to create new 'MSF's that include an
-- additional layer, and functions to flatten that layer out of the 'MSF`'s
-- transformer stack.
--
-- It is based on the _strict_ state monad 'Control.Monad.Trans.State.Strict',
-- so when combining it with other modules such as @mtl@'s,
-- the strict version has to be included, i.e. 'Control.Monad.State.Strict'
-- instead of 'Control.Monad.State' or 'Control.Monad.State.Lazy'.
module Control.Monad.Trans.MSF.State
  ( module Control.Monad.State.Trans
  -- * 'State' 'MSF' running and wrapping
  , stateS
  , runStateS
  , runStateS_
  , execStateS
  ) where

import Control.Monad.State.Trans (class MonadState, class MonadTrans, StateT(..), evalStateT, execStateT, get, gets, lift, mapStateT, modify, modify_, put, runStateT, state, withStateT)
import Data.MonadicStreamFunction.InternalCore (MSF, feedback, morphGS)
import Prelude

import Data.Profunctor (arr)
import Data.Tuple (Tuple(..), snd, swap)

-- * 'State' 'MSF' running and wrapping

-- | Build an 'MSF' in the 'State' monad from one that takes the state as an
-- extra input. This is the opposite of 'runStateS'.
stateS :: forall m a b s. Monad m => MSF m (Tuple s a) (Tuple s b) -> MSF (StateT s m) a b
stateS = morphGS ( \f a -> StateT (\s -> (\(Tuple (Tuple s' b) c) -> (Tuple (Tuple b c) s')) <$> f (Tuple s a) ))

-- | Build an 'MSF' that takes a state as an extra input from one on the
-- 'State' monad. This is the opposite of 'stateS'.
runStateS :: forall m a b s. Monad m => MSF (StateT s m) a b -> MSF m (Tuple s a) (Tuple s b)
runStateS = morphGS ( \f (Tuple s a) -> (\(Tuple (Tuple b c) s') -> (Tuple (Tuple s' b) c))
        <$> runStateT (f a) s )

-- | Build an 'MSF' /function/ that takes a fixed state as additional input,
-- from an 'MSF' in the 'State' monad, and outputs the new state with every
-- transformation step.
runStateS_ :: forall m a b s. Monad m => MSF (StateT s m) a b -> s -> MSF m a (Tuple s b)
runStateS_ msf s = feedback s
                 $ arr swap >>> runStateS msf >>> arr (\(Tuple s' b) -> (Tuple (Tuple s' b) s'))

-- TODO Rename this to execStateS!
-- | Build an 'MSF' /function/ that takes a fixed state as additional
-- input, from an 'MSF' in the 'State' monad.
execStateS :: forall m a b s. Monad m => MSF (StateT s m) a b -> s -> MSF m a b
execStateS msf s = runStateS_ msf s >>> arr snd
