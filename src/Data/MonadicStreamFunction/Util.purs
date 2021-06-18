module Data.MonadicStreamFunction.Util where
  
import Prelude (class Monad, Unit, const, identity, unit, ($), (+), (<<<), (>>>))
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Profunctor (arr)
import Data.Profunctor.Choice ((|||))
import Data.Integral (class Integral, fromIntegral)
import Data.Tuple (Tuple(..))


import Data.MonadicStreamFunction.InternalCore (MSF, feedback)

-- * Streams and sinks

-- | A stream is an 'MSF' that produces outputs, while ignoring the input.
-- It can obtain the values from a monadic context.
type MStream m a = MSF m Unit a

-- | A sink is an 'MSF' that consumes inputs, while producing no output.
-- It can consume the values with side effects.
type MSink   m a = MSF m a Unit

-- | Apply an 'MSF' to every input. Freezes temporarily if the input is
-- 'Nothing', and continues as soon as a 'Just' is received.
mapMaybeS :: forall m a b. Monad m => MSF m a b -> MSF m (Maybe a) (Maybe b)
mapMaybeS msf
  = (arr
       (\ maybeA ->
          case maybeA of
              Just a -> (Left) (a)
              Nothing -> (Right) (unit))
       >>>
       (((arr (\ a -> a)) >>> (arr Just <<< msf)) |||
          ((arr (\ unit -> Nothing)) >>> (identity))))

-- * Folding

-- ** Folding for 'VectorSpace' instances

-- | Count the number of simulation steps. Produces 1, 2, 3,...
count :: forall n m a. Integral n => Monad m => MSF m a n
count = arr (const $ fromIntegral 1) >>> accumulateWith (+) (fromIntegral 0)

-- ** Generic folding \/ accumulation

-- | Applies a function to the input and an accumulator,
-- outputting the updated accumulator.
-- Equal to @\f s0 -> feedback s0 $ arr (uncurry f >>> dup)@.
accumulateWith :: forall m a s. Monad m => (a -> s -> s) -> s -> MSF m a s
accumulateWith f s0 = feedback s0 $ arr g
  where
    g (Tuple a s) = let s' = f a s in (Tuple s' s')