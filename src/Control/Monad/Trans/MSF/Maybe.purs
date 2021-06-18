-- Transformed from hakell proc notation with https://github.com/pepeiborra/arrowp
{- |
The 'Maybe' monad is very versatile. It can stand for default arguments,
for absent values, and for (nondescript) exceptions.
The latter viewpoint is most natural in the context of 'MSF's.
-}
module Control.Monad.Trans.MSF.Maybe
       (module Control.Monad.Trans.MSF.Maybe,
        module Control.Monad.Maybe.Trans)
       where

import Prelude
import Data.Tuple (Tuple(..))
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..), either)
import Data.Profunctor (arr)
import Data.Profunctor.Strong (first)
import Data.Profunctor.Choice ((|||))

import Control.Monad.Maybe.Trans (class MonadTrans, MaybeT(..), lift, mapMaybeT, runMaybeT)
import Control.Monad.Trans.MSF.Except (ExceptT, exceptS, listToMSFExcept, maybeToExceptS, runExceptT, runMSFExcept, safe, safely, try)
import Data.MonadicStreamFunction (MSF(..), arrM, constM, liftTransS, morphS)

exit :: forall m a b. Monad m => MSF (MaybeT m) a b
exit = constM $ MaybeT $ pure Nothing

exitWhen ::forall m a.  Monad m => (a -> Boolean) -> MSF (MaybeT m) a a
exitWhen condition
  = (arr (\ a -> Tuple a a) >>>
       ((first ((arr (\ a -> condition a)) >>> (exitIf))) >>>
          (arr (\(Tuple _ a) -> a)) >>> identity))

exitIf :: forall m. Monad m => MSF (MaybeT m) Boolean Unit
exitIf
  = (arr
       (\ condition -> if condition then (Left) unit else (Right) unit)
       >>>
       (((arr (\ unit -> unit)) >>> (exit)) |||
          ((arr (\ unit -> unit)) >>> identity)))

maybeExit :: forall m a. Monad m => MSF (MaybeT m) (Maybe a) a
maybeExit = inMaybeT

inMaybeT :: forall m a. Monad m => MSF (MaybeT m) (Maybe a) a
inMaybeT = arrM $ MaybeT <<< pure


-- * Catching Maybe exceptions

-- | Run the first @msf@ until the second one produces 'True' from the output of the first.
untilMaybe :: forall m a b. 
             Monad m => MSF m a b -> MSF m b Boolean -> MSF (MaybeT m) a b
untilMaybe msf cond
  = (arr (\ a -> a) >>>
       ((liftTransS msf) >>>
          (arr (\ b -> Tuple b b)) >>>
            (first ((arr (\ b -> b)) >>> (liftTransS cond))) >>>
              (arr (\ (Tuple c b) -> if c then Nothing else Just b)) >>> (inMaybeT)))

untilMaybe_ :: forall m a b. 
             Monad m => MSF m a b -> MSF m b Boolean -> MSF (MaybeT m) a Unit
untilMaybe_ msf cond
  = (arr (\ a -> a) >>>
       ((liftTransS msf) >>>
          (arr (\ b -> Tuple b b)) >>>
            (first ((arr (\ b -> b)) >>> (liftTransS cond))) >>>
              (arr (\ (Tuple c b) -> if c then Nothing else Just unit)) >>> (inMaybeT)))

-- | When an exception occurs in the first 'msf', the second 'msf' is executed from there.
catchMaybe :: forall m a b. Monad m =>
             MSF (MaybeT m) a b -> MSF m a b -> MSF m a b
catchMaybe msf1 msf2
  = safely $
      do _ <- try $ maybeToExceptS msf1
         safe msf2

-- * Converting to and from 'MaybeT'

-- | Convert exceptions into `Nothing`, discarding the exception value.
exceptToMaybeS :: forall e m a b. Monad m => MSF (ExceptT e m) a b -> MSF (MaybeT m) a b
exceptToMaybeS
  = morphS (MaybeT <<< map (either (const Nothing) Just) <<< runExceptT)

-- | Converts a list to an 'MSF' in 'MaybeT',
--   which outputs an element of the list at each step,
--   throwing 'Nothing' when the list ends.
listToMaybeS :: forall m a b. Monad m  => List b -> MSF (MaybeT m) a b
listToMaybeS = exceptToMaybeS <<< runMSFExcept <<< listToMSFExcept

-- * Running 'MaybeT'
-- | Remove the 'MaybeT' layer by outputting 'Nothing' when the exception occurs.
--   The continuation in which the exception occurred is then tested on the next input.
runMaybeS :: forall m a b. Monad m  => MSF (MaybeT m) a b -> MSF m a (Maybe b)
runMaybeS msf = exceptS (maybeToExceptS msf) >>> arr eitherToMaybe
  where eitherToMaybe (Left unit) = Nothing
        eitherToMaybe (Right b) = Just b

-- runMaybeMSF :: forall m a b. Monad m  => MSF (MaybeT m) a b -> MSF m a (Maybe b)
-- runMaybeMSF = arrM ?eitherToMaybe
--   where eitherToMaybe (Left unit) = Nothing
--         eitherToMaybe (Right b) = Just b

-- | Reactimates an 'MSF' in the 'MaybeT' monad until it throws 'Nothing'.
reactimateMaybe :: forall m. Monad m  => MSF (MaybeT m) Unit Unit -> m Unit
-- reactimateMaybe msf = reactimateExcept $ try $ maybeToExceptS msf
reactimateMaybe (MSF sf) = do
  maybeMsf <- runMaybeT $ sf unit 
  case maybeMsf of
    Nothing -> pure unit
    Just (Tuple _ sf') -> reactimateMaybe sf'

-- -- | Run an 'MSF' indefinitely passing a unit-carrying input stream.
-- reactimate :: forall m. Monad m => MSF m Unit Unit -> m Unit
-- reactimate (MSF sf) = do
--   Tuple _ sf' <- sf unit
--   reactimate sf'

-- | Run an 'MSF' fed from a list, discarding results. Useful when one needs to
-- combine effects and streams (i.e., for testing purposes).
embed_ :: forall m a . Functor m => Monad m  => MSF m a Unit -> List a -> m Unit
embed_ msf as
  = reactimateMaybe $ listToMaybeS as >>> liftTransS msf