module Data.MonadicStreamFunction.InternalCore where

import Prelude (class Category, class Monad, class Semigroupoid, Unit, bind, compose, pure, unit, ($))
import Data.Tuple (Tuple(..))
import Data.Either (Either(..))
import Data.List (List(..), (:))


import Data.Profunctor (class Profunctor, dimap)
import Data.Profunctor.Strong (class Strong, first, second)
import Data.Profunctor.Choice (class Choice, left, right)
-- * Definitions

-- | Stepwise, side-effectful 'MSF's without implicit knowledge of time.
--
-- 'MSF's should be applied to streams or executed indefinitely or until they
-- terminate. See 'reactimate' and 'reactimateB' for details. In general,
-- calling the value constructor 'MSF' or the function 'unMSF' is discouraged.

data MSF m a b = MSF (a -> m (Tuple b (MSF m a b )))

unMSF :: forall m a b. MSF m a b -> (a -> m (Tuple b (MSF m a b)))
unMSF (MSF msf) = msf

-- Instances

-- | Instance definition for 'Category'. Defines 'id' and '.'.
instance categoryMSF :: Monad m => Category (MSF m) where
  identity = go
    where go = MSF $ \a -> pure $ Tuple a go
  -- arr = composeKleisli

instance semigroupoidMSF :: Monad m => Semigroupoid (MSF m) where
  -- compose 
  --   :: forall a b c m
  --   .  MSF m b c -- (a -> m (Tuple b (MSF m a b))) 
  --   -> MSF m a b -- (b -> m (Tuple c (MSF m b c))) 
  --   -> MSF m a c -- (a -> m (Tuple c (MSF m a c)))
  compose (MSF sf2) (MSF sf1) = MSF $ \a -> do
    Tuple b (MSF sf1') <- sf1 a
    Tuple c (MSF sf2') <- sf2 b
    let sf' = compose (MSF sf2') (MSF sf1')
    pure $ Tuple c sf'

instance profunctorMSF :: Monad m => Profunctor (MSF m) where
-- dimap :: forall a b c d. (a -> b) -> (c -> d) -> ( b -> m (Tuple c (MSF...) ) -> MSF m a d
-- dimap ab cd msf = (arr cd) <<< msf <<< (arr ab)

  dimap :: forall a b c d m. Monad m => (a -> b) -> (c -> d) -> MSF m b c -> MSF m a d
  dimap ab cd (MSF mbc) = MSF $ \a -> do 
    Tuple c (MSF mbc') <- mbc (ab a)
    --  Tuple c (MSF (msfbc2 :: ?a))
    pure $ Tuple (cd c) $ dimap ab cd (MSF mbc')
  -- dimap ab cd msf = (arr cd) <<< msf <<< (arr ab) 

--   dimap :: forall a b c d. (a -> b) -> (c -> d) -> MSF m b c -> MSF m a d
--   -- dimap :: forall a b c d. (a -> b) -> (c -> d) -> ( b -> m (Tuple c (MSF...) ) -> MSF m a d
--   dimap ab cd (MSF msfbc) = MSF $ \a -> 
--     msfbc (ab a) >>= ?a
-- -- /    pure $ cd ?c

instance strongMSF :: Monad m => Strong (MSF m) where
--   -- first :: forall a b c p. p a b -> p (Tuple a c) (Tuple b c)
--   -- first = morphGS $ \f (Tuple a c) -> do
--   --           Tuple b msf' <- f a
--   --           pure $ Tuple (Tuple b c) ?msf'
  first :: forall a b c. MSF m a b -> MSF m (Tuple a c) (Tuple b c)
  first (MSF mab) = MSF $ \(Tuple a c) -> do
    Tuple b mab' <- mab a
    pure $ Tuple (Tuple b c) (first mab')

  second :: forall a b c. MSF m b c -> MSF m (Tuple a b) (Tuple a c)
  second (MSF mbc) = MSF $ \(Tuple a b) -> do
    Tuple c mbc' <- mbc b
    pure $ Tuple (Tuple a c) (second mbc')

instance choiceMSF :: Monad m => Choice (MSF m) where
  -- left :: forall m a b c. MSF m a b -> MSF m (Either a c) (Either b c)
  left (MSF sf) = MSF $ \x -> case x of
      (Left a) -> do Tuple b sf' <- sf a
                     pure $ Tuple (Left b) (left sf')
      (Right c) -> pure $ Tuple (Right c) (left (MSF sf))
    -- where
    --   f :: forall m a b c. Either a c -> MSF m (Either a c) (Either b c)
    --   f (Left a) = do Tuple b (MSF sf') <- sf a
    --                   pure $ Tuple (Left b) (left sf')
    --   f (Right c) = pure $ Tuple (Right c) (left sf)

  -- right :: forall m a b c. MSF m a b -> MSF m (Either a c) (Either b c)
  right (MSF sf) = MSF f
    where
      f (Right a) = do Tuple b sf' <- sf a
                       pure $ Tuple (Right b) (right sf')
      f (Left c) = pure $ Tuple (Left c) (right (MSF sf))


-- * Monadic computations and 'MSF's

-- | Generic lifting of a morphism to the level of 'MSF's.
--
-- Natural transformation to the level of 'MSF's.
--
-- __Mathematical background:__ The type @a -> m (b, c)@ is a functor in @c@,
-- and @MSF m a b@ is its greatest fixpoint, i.e. it is isomorphic to the type
-- @a -> m (b, MSF m a b)@, by definition.
-- The types @m@, @a@ and @b@ are parameters of the functor.
-- Taking a fixpoint is functorial itself, meaning that a morphism
-- (a natural transformation) of two such functors gives a morphism
-- (an ordinary function) of their fixpoints.
--
-- This is in a sense the most general "abstract" lifting function,
-- i.e. the most general one that only changes input, output and side effect
-- types, and doesn't influence control flow.
-- Other handling functions like exception handling or 'ListT' broadcasting
-- necessarily change control flow.

morphGS :: forall a1 b1 a2 b2 m1 m2. Monad m2
        => (forall c . (a1 -> m1 (Tuple b1 c)) -> (a2 -> m2 (Tuple b2 c)))
          -- ^ The natural transformation. @mi@, @ai@ and @bi@ for @i = 1, 2@
          --   can be chosen freely, but @c@ must be universally quantified
        -> MSF m1 a1 b1
        -> MSF m2 a2 b2
morphGS morph (MSF msf) = MSF $ \a2 -> do
  Tuple b2 msf' <- morph msf a2
  pure $ Tuple b2 (morphGS morph msf')

-- * Feedback loops

-- | Well-formed looped connection of an output component as a future input.
feedback :: forall m a b c. Monad m => c -> MSF m (Tuple a c) (Tuple b c) -> MSF m a b
feedback c sf = MSF $ \a -> do
  (Tuple (Tuple b' c') sf') <- unMSF sf (Tuple a c)
  pure (Tuple b' (feedback c' sf'))

-- * Execution/simulation

-- | Apply a monadic stream function to a list.
--
-- Because the result is in a monad, it may be necessary to
-- traverse the whole list to evaluate the value in the results to WHNF.
-- For example, if the monad is the maybe monad, this may not produce anything
-- if the 'MSF' produces 'Nothing' at any point, so the output stream cannot
-- consumed progressively.
--
-- To explore the output progressively, use 'arrM' and '(>>>)'', together
-- with some action that consumes/actuates on the output.
--
-- This is called 'runSF' in Liu, Cheng, Hudak, "Causal Commutative Arrows and
-- Their Optimization"
embed :: forall a b m . Monad m => MSF m a b -> List a -> m (List b)
embed _  Nil     = pure Nil
embed (MSF sf) (a:as) = do
  Tuple b sf' <- sf a
  bs       <- embed sf' as
  pure (b:bs)

-- | Run an 'MSF' indefinitely passing a unit-carrying input stream.
reactimate :: forall m. Monad m => MSF m Unit Unit -> m Unit
reactimate (MSF sf) = do
  Tuple _ sf' <- sf unit
  reactimate sf'