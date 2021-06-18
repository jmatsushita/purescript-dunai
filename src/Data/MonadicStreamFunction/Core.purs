module Data.MonadicStreamFunction.Core 
  ( module Data.MonadicStreamFunction.Core 
  , module Export)
  where

import Prelude

import Control.Monad.Base (class MonadBase, liftBase)
import Control.Monad.Trans.Class (class MonadTrans, lift)

import Data.Tuple (Tuple(..))


import Data.MonadicStreamFunction.InternalCore (MSF(..), morphGS)
import Data.MonadicStreamFunction.InternalCore (MSF(..)) as Export

-- ** Lifting point-wise computations

-- | Lifts a monadic computation into a Stream.
constM :: forall a b m. Monad m => m b -> MSF m a b
constM = arrM <<< const

-- | Apply a monadic transformation to every element of the input stream.
--
-- Generalisation of 'arr' from 'Arrow' to monadic functions.
arrM :: forall a b m. Monad m => (a -> m b) -> MSF m a b
-- arrM f = morphGS (\i a -> i a >>= \(_,c) -> f a >>= \b -> return (b, c)) C.id
arrM f = go
 where go = MSF $ \a -> do
              b <- f a
              pure $ Tuple b go

-- | Monadic lifting from one monad into another
liftBaseM :: forall a b m1 m2. Monad m2 => MonadBase m1 m2 => (a -> m1 b) -> MSF m2 a b
liftBaseM = arrM <<< (compose liftBase)

-- ** MSF combinators that apply monad transformations

-- | Lift innermost monadic actions in monad stack (generalisation of
-- 'liftIO').
liftBaseS :: forall a b m1 m2. Monad m2 => MonadBase m1 m2 => MSF m1 a b -> MSF m2 a b
liftBaseS = morphS liftBase

-- *** MonadBase
-- | Lift the first 'MSF' into the monad of the second.
liftFirst :: forall a b c m1 m2. MonadBase m1 m2 => MSF m1 a b -> MSF m2 b c -> MSF m2 a c
liftFirst sf1 sf2 = liftBaseS sf1 >>> sf2

infixr 0 liftFirst as ^>>>

-- | Lift the second 'MSF' into the monad of the first.
liftSecond  :: forall a b c m1 m2. MonadBase m1 m2 => MSF m2 a b -> MSF m1 b c -> MSF m2 a c
liftSecond sf1 sf2 = sf1 >>> liftBaseS sf2

infixr 0 liftSecond as >>>^

-- *** MonadTrans

-- | Lift inner monadic actions in monad stacks.

liftTransS :: forall a b t m
            . MonadTrans t
           => Monad m
           => Monad (t m)
           => MSF m a b
           -> MSF (t m) a b
liftTransS = morphS lift

-- *** Generic monadic transformation

-- | Apply trans-monadic actions (in an arbitrary way).
--
-- This is just a convenience function when you have a function to move across
-- monads, because the signature of 'morphGS' is a bit complex.
morphS :: forall a b m1 m2
        . Monad m2
      =>  Monad m1
      => (m1 ~> m2)
      -> MSF m1 a b
      -> MSF m2 a b
morphS morph = morphGS morph'
  where
    -- The following makes the a's and the b's the same, and it just says:
    -- whatever function m1F you give me to apply to every sample, I use morph
    -- on the result to go from m1 to m2.
    --
    -- Remember that:
    -- morphGS :: Monad m2
    --         => (forall c . (a1 -> m1 (b1, c)) -> (a2 -> m2 (b2, c)))
    --           -- ^ The natural transformation. @mi@, @ai@ and @bi@ for @i = 1, 2@
    --           --   can be chosen freely, but @c@ must be universally quantified
    --         -> MSF m1 a1 b1
    --         -> MSF m2 a2 b2
    --
    morph' :: (forall c . (a -> m1 (Tuple b c)) -> (a -> m2 (Tuple b c)))
    morph' m1F = morph <<< m1F
