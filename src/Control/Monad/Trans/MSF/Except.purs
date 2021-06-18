module Control.Monad.Trans.MSF.Except
       (module Control.Monad.Trans.MSF.Except,
        module Control.Monad.Except.Trans)
       where

import Control.Monad.Except.Trans (class MonadError, class MonadThrow, class MonadTrans, ExceptT(..), catchError, except, lift, mapExceptT, runExceptT, throwError, withExceptT)
import Control.Monad.Maybe.Trans (MaybeT, runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.List (List)
import Data.Maybe (Maybe(..), maybe)
import Data.MonadicStreamFunction (arrM, constM, count, liftTransS, mapMaybeS, morphS)
import Data.MonadicStreamFunction.InternalCore (MSF(..), reactimate, unMSF)
import Data.Profunctor (arr)
import Data.Profunctor.Strong (first)
import Data.Tuple (Tuple(..), fst)
import Partial.Unsafe (unsafeCrashWith)
import Prelude

import Data.Profunctor.Choice ((|||))

throwOnCond :: forall e m a. Monad m => (a -> Boolean) -> e -> MSF (ExceptT e m) a a
throwOnCond cond e
  = (arr (\ a -> if cond a then (Left) unit else (Right) (a)) >>>
       (((arr (\ unit -> e)) >>> (throwS)) |||
          ((arr (\ a -> a)) >>> identity)))

throwOnCondM :: forall e m a. 
               Monad m => (a -> m Boolean) -> e -> MSF (ExceptT e m) a a
throwOnCondM cond e
  = (arr (\ a -> Tuple a a) >>>
       ((first ((arr (\ a -> a)) >>> (arrM (lift <<< cond)))) >>>
          (arr (\(Tuple b a) -> if b then (Left) (unit) else (Right) (a))) >>>
            (((arr (\ unit -> e)) >>> (throwS)) |||
               ((arr (\ a -> a)) >>> identity))))

throwOn :: forall e m. Monad m => e -> MSF (ExceptT e m) Boolean Unit
throwOn e = (arr (\ b -> Tuple b e) >>> throwOn')

throwOn' :: forall e m. Monad m => MSF (ExceptT e m) (Tuple Boolean e) Unit
throwOn'
  = (arr (\(Tuple b e) -> if b then (Left) (e) else (Right) (unit)) >>>
       (((arr (\ e -> e)) >>> (throwS)) |||
          ((arr (\ unit -> unit)) >>> identity)))

throwMaybe :: forall e m a. Monad m => MSF (ExceptT e m) (Maybe e) (Maybe a)
throwMaybe = mapMaybeS throwS

throwS :: forall e m a. Monad m => MSF (ExceptT e m) e a
throwS = arrM throwError

throw :: forall e m a b. Monad m => e -> MSF (ExceptT e m) a b
throw = constM <<< throwError

pass :: forall e m a. Monad m => MSF (ExceptT e m) a a
pass = identity


-- | Converts an 'MSF' in 'MaybeT' to an 'MSF' in 'ExceptT'.
--   Whenever 'Nothing' is thrown, throw @()@ instead.
maybeToExceptS :: forall m a b
                . Monad m 
               => MSF (MaybeT m) a b 
               -> MSF (ExceptT Unit m) a b
maybeToExceptS = morphS (ExceptT <<< m <<< runMaybeT)
  where
    m :: forall c. Monad m => m (Maybe c) -> m (Either Unit c)
    m a = maybe (Left unit) Right <$> a
-- maybeToExceptS = morphS (ExceptT . (maybe (Left ()) Right <$>) . runMaybeT)

-- * Catching exceptions

-- | Catch an exception in an 'MSF'. As soon as an exception occurs,
--   the current continuation is replaced by a new 'MSF', the exception handler,
--   based on the exception value.
--   For exception catching where the handler can throw further exceptions,
--   see 'MSFExcept' further below.
catchS :: forall e m a b . 
         Monad m => MSF (ExceptT e m) a b -> (e -> MSF m a b) -> MSF m a b
catchS msf f
  = safely $
      do e <- try msf
         safe $ f e

untilE :: forall e m a b.
         Monad m => MSF m a b -> MSF m b (Maybe e) -> MSF (ExceptT e m) a b
untilE msf msfe
  = (arr (\ a -> a) >>>
       ((liftTransS msf) >>>
          (arr (\ b -> Tuple b b)) >>>
            (first ((arr (\ b -> b)) >>> (liftTransS msfe))) >>>
              (arr (\ (Tuple me b) -> ExceptT $ pure $ maybe (Right b) Left me)) >>>
                (inExceptT)))

-- exceptS :: (Functor m, Monad m) => MSF (ExceptT e m) a b -> MSF m a (Either e b)
-- exceptS = transG return $ const $ fmap f . runExceptT
--   where
--     f (Left e)       = (Left e , Nothing)
--     f (Right (b, c)) = (Right b, Just c )

-- transG ::  forall m1 m2 a1 a2 b1 b2
--         .  Monad m1 
--         => Monad m2 
--         => (a2 -> m1 a1) 
--         -> (forall c. a2 -> m1 (Tuple b1 c) -> m2 (Tuple b2 (Maybe c))) 
--         -> MSF m1 a1 b1 
--         -> MSF m2 a2 b2

-- TODO This needs to be renamed as 'runExceptS'!
-- 'exceptS' would have type @MSF m a (Either e b) -> MSF (ExceptT e m) a b@
-- | Escape an 'ExceptT' layer by outputting the exception whenever it occurs.
--   If an exception occurs, the current 'MSF' continuation is tested again
--   on the next input.
exceptS :: forall e m a b
        .  Functor m 
        => Monad m 
        => MSF (ExceptT e m) a b 
        -> MSF m a (Either e b)

exceptS = transG 
  pure 
  goTransG 

goTransG :: forall e m a b c. Monad m => a -> ExceptT e m (Tuple b c) -> m (Tuple (Either e b) (Maybe c))
goTransG = const (map f <<< runExceptT)
  where
    f (Left e) = Tuple (Left e) Nothing
    f (Right (Tuple b c)) = Tuple (Right b) (Just c)


inExceptT :: forall e m a. Monad m => MSF (ExceptT e m) (ExceptT e m a) a
inExceptT = arrM identity

tagged :: forall e1 e2 m a b. 
         Monad m => MSF (ExceptT e1 m) a b -> MSF (ExceptT e2 m) (Tuple a e2) b
tagged msf
  = runMSFExcept $
      do _ <- try $ msf <<< arr fst
         Tuple _ e2 <- currentInput
         pure e2

-- * Monad interface for Exception MSFs

-- | 'MSF's with an 'ExceptT' transformer layer
--   are in fact monads /in the exception type/.
--
--   * 'return' corresponds to throwing an exception immediately.
--   * '>>=' is exception handling:
--     The first value throws an exception,
--     while the Kleisli arrow handles the exception
--     and produces a new signal function,
--     which can throw exceptions in a different type.
--   * @m@: The monad that the 'MSF' may take side effects in.
--   * @a@: The input type
--   * @b@: The output type
--   * @e@: The type of exceptions that can be thrown
newtype MSFExcept m a b e = MSFExcept (MSF (ExceptT e m) a b)

runMSFExcept :: forall m a b e. MSFExcept m a b e -> MSF (ExceptT e m) a b
runMSFExcept (MSFExcept msfe) = msfe

try :: forall e m a b. MSF (ExceptT e m) a b -> MSFExcept m a b e
try = MSFExcept

currentInput :: forall e m b. Monad m => MSFExcept m e b e
currentInput = try throwS

instance functorMSFExcept :: Monad m => Functor (MSFExcept m a b) where
  map = liftM1

instance applyMSFExcept :: Monad m => Apply (MSFExcept m a b) where
  apply = ap
  -- apply :: forall a b. f (a -> b) -> f a -> f b
  -- -- apply :: forall a b. MSF (ExceptT (e -> e') m) a b -> MSF (ExceptT e m) a b -> MSF (ExceptT e' m) a b
  -- apply (MSFExcept msfee') (MSFExcept msfe) = 

instance applicativeMSFExcept :: Monad m => Applicative (MSFExcept m a b) where
  pure = MSFExcept <<< throw

instance bindMSFExcept :: Monad m => Bind (MSFExcept m a b) where
  bind (MSFExcept msf) f
    = MSFExcept $ handleExceptT msf $ runMSFExcept <<< f

-- | Monad instance for 'MSFExcept'. Bind uses the exception as the 'return'
-- value in the monad.
instance monadMSFExcept :: Monad m => Monad (MSFExcept m a b)

handleExceptT :: forall e1 e2 m a b. 
                Monad m =>
                MSF (ExceptT e1 m) a b ->
                  (e1 -> MSF (ExceptT e2 m) a b) -> MSF (ExceptT e2 m) a b
handleExceptT msf f
  = flip handleGen msf $
      \ a mbcont ->
        do ebcont <- lift $ runExceptT mbcont
           case ebcont of
               Left e -> unMSF (f e) a
               Right (Tuple b msf') -> pure $ Tuple b (handleExceptT msf' f)

data Empty

-- | A function that extracts the value from the `Left` data constructor.
-- | The first argument is a default value, which will be returned in the
-- | case where a `Right` is passed to `fromLeft`.
fromLeft :: forall a b. a -> Either a b -> a
fromLeft _ (Left a) = a
fromLeft default _ = default

-- | A function that extracts the value from the `Right` data constructor.
-- | The first argument is a default value, which will be returned in the
-- | case where a `Left` is passed to `fromRight`.
fromRight :: forall a b. b -> Either a b -> b
fromRight _ (Right b) = b
fromRight default _ = default

-- | If no exception can occur, the 'MSF' can be executed without the 'ExceptT' layer.
safely :: forall m a b. Monad m => MSFExcept m a b Empty -> MSF m a b
safely (MSFExcept msf) = morphS fromExcept msf
  where
    fromExcept :: forall c. Monad m => ExceptT Empty m c -> m c
    -- We can assume that the pattern @Left e@ will not occur,
    -- since @e@ would have to be of type @Empty@.    
    fromExcept ma = do 
      rightMa <- runExceptT ma
      pure $ fromRight (unsafeCrashWith "safely: Received `Left`") rightMa

-- safely :: Monad m => MSFExcept m a b Empty -> MSF m a b
-- safely (MSFExcept msf) = morphS fromExcept msf
--   where
--     -- We can assume that the pattern @Left e@ will not occur,
--     -- since @e@ would have to be of type @Empty@.
--     fromExcept ma = do
--       rightMa <- runExceptT ma
--       return $ fromRight (error "safely: Received `Left`") rightMa

-- | An 'MSF' without an 'ExceptT' layer never throws an exception,
--   and can thus have an arbitrary exception type.
safe :: forall e m a b. Monad m => MSF m a b -> MSFExcept m a b e
safe = try <<< liftTransS

once :: forall e m a b. Monad m => (a -> m e) -> MSFExcept m a b e
once f = try $ arrM (lift <<< f) >>> throwS

once_ :: forall e m a b. Monad m => m e -> MSFExcept m a b e
once_ = once <<< const

step :: forall e m a b. Monad m => (a -> m (Tuple b e)) -> MSFExcept m a b e
step f
  = try $
      (arr (\ a -> (Tuple unit a)) >>>
         ((first ((arr (\ unit -> unit)) >>> (count))) >>>
            (arr (\ (Tuple n a) -> (Tuple a n))) >>>
              (first ((arr (\ a -> a)) >>> (arrM (lift <<< f)))) >>>
                (arr (\ (Tuple (Tuple b e) n) -> (Tuple (Tuple e n) b))) >>>
                  (first ((arr (\(Tuple e n) -> Tuple (n > (1 :: Int)) e)) >>> (throwOn')))
                    >>> (arr (\(Tuple _ b) -> b)) >>> identity))

step_ :: forall m a b.  Monad m => b -> MSFExcept m a b Unit
step_ b = step $ const $ pure $ (Tuple b unit)

listToMSFExcept :: forall m a b. Monad m => List b -> MSFExcept m a b Unit
listToMSFExcept = traverse_ step_

performOnFirstSample :: forall m a b. Monad m => m (MSF m a b) -> MSF m a b
performOnFirstSample sfaction
  = safely $
      do msf <- once_ sfaction
         safe msf

reactimateExcept :: forall e m. Monad m => MSFExcept m Unit Unit e -> m e
reactimateExcept msfe
  = do leftMe <- runExceptT $ reactimate $ runMSFExcept msfe
       pure $
         fromLeft (unsafeCrashWith "reactimateExcept: Received `Right`") leftMe

reactimateB :: forall m. Monad m => MSF m Unit Boolean -> m Unit
reactimateB sf
  = reactimateExcept $ try $ liftTransS sf >>> throwOn unit

switch :: forall m a b c. 
         Monad m => MSF m a (Tuple b (Maybe c)) -> (c -> MSF m a b) -> MSF m a b
switch sf f = catchS ef f
  where ef
          = (arr (\ a -> a) >>>
               ((liftTransS sf) >>>
                  (arr (\ (Tuple b me) -> ExceptT $ pure $ maybe (Right b) Left me)) >>>
                    (inExceptT)))

transG ::  forall m1 m2 a1 a2 b1 b2
        .  Monad m1 
        => Monad m2 
        => (a2 -> m1 a1) 
        -> (forall c. a2 -> m1 (Tuple b1 c) -> m2 (Tuple b2 (Maybe c))) 
        -> MSF m1 a1 b1 
        -> MSF m2 a2 b2
transG transformInput transformOutput msf = go
  where go
          = MSF $
              \ a2 ->
                do (Tuple b2 msf') <- transformOutput a2 $
                                   unMSF msf =<< transformInput a2
                   case msf' of
                       Just msf'' -> pure
                                       $ Tuple b2 $ transG transformInput transformOutput msf''
                       Nothing -> pure $ Tuple b2 go

handleGen :: forall m1 m2 a b1 b2.
          (a -> m1 (Tuple b1 (MSF m1 a b1)) -> m2 (Tuple b2 (MSF m2 a b2))) ->
            MSF m1 a b1 -> MSF m2 a b2
handleGen handler msf = MSF $ \ a -> handler a (unMSF msf a)