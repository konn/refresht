{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, KindSignatures                #-}
{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables                #-}
{-# LANGUAGE StandaloneDeriving, TemplateHaskell                       #-}
module Control.Monad.Refresh
       ( -- * @'RefreshT'@ type and settings
         RefreshT, runRefreshT, evalRefreshT
       , -- ** Settings for Refreshing
         RefreshSetting
       , defaultRefreshSetting
       , refresher
         -- | Environment refreshing function (default: @return@).
         --
         --   Since 0.1.0.0
       , refreshDelay
         -- | Delay in microseconds before environmental refreshment (default: @100ms@).
         --
         --   Since 0.1.0.0
       , shouldRefresh
         -- | Condition to determine if environment should be refreshed (default: @const True@).
         --
         --   Since 0.1.0.0
       , isRefreshingError
         -- | If this exception should occur an envionment refreshment? (default: refresh for any exception).
         --
         --   Since 0.1.0.0
       , -- * Transaction Combinators
         atomic
       , atomicLift
       , atomicLiftIO
       , withEnv
       , refresh
       ) where
import Control.Concurrent  (threadDelay)
import Control.Exception   (SomeException (..))
import Control.Lens        (makeLenses, view, (^.))
import Control.Monad.Catch (MonadCatch (..), MonadThrow (..), catchIf)
import Control.Monad.RWS   (MonadTrans (..), RWST (..), ask, evalRWST, get,
                            gets)
import Control.Monad.RWS   (MonadIO (liftIO), MonadReader (..), modify, runRWST)
import Data.Default        (Default (..))
import Data.Typeable       (Typeable)

-- | Settings for Refreshment
--
--   Since 0.1.0.0
data RefreshSetting s m =
  RefreshSetting { _refresher         :: s -> m s
                 , _refreshDelay      :: Int
                 , _isRefreshingError :: SomeException -> Bool
                 , _shouldRefresh     :: s -> m Bool
                 }
  deriving (Typeable)

-- | Since 0.1.0.0
instance Monad m => Default (RefreshSetting s m) where
  def = RefreshSetting return (10 ^ 5) (const True) (const $ return True)

defaultRefreshSetting :: Monad m => RefreshSetting s m
defaultRefreshSetting = def

makeLenses ''RefreshSetting

data Localed a = Localed { modifier :: !(a -> a)
                         , original :: !a
                         }

runLocaled :: forall t. Localed t -> t
runLocaled (Localed f a) = f a

-- | Reader monad transformer with an automatic environment refreshment.
--
--   Since 0.1.0.0
newtype RefreshT s m a =
  RefreshT { runRefreshT_ :: RWST (RefreshSetting s m) () (Localed s) m a }
  deriving (Functor, Applicative, Monad)

-- | N.B. The @'lift'@ combinator doesn't care about exceptions;
--        this is the intended behaviour, because @'lift'@ doesn't
--        come with any atomicity meaning.
--        If you want to trigger refresh after exceptions, use @'atomicLift'@.
--
--   Since 0.1.0.0
instance MonadTrans (RefreshT s) where
  lift = RefreshT . lift

-- | N.B. The @'liftIO'@ combinator doesn't care about exceptions;
--        this is the intended behaviour, because @'liftIO'@ doesn't
--        come with any atomicity meaning.
--        If you want to trigger refresh after exceptions, use @'atomicLiftIO'@.
--
--   Since 0.1.0.0
instance (MonadIO m) => MonadIO (RefreshT s m) where
  liftIO = RefreshT . liftIO

-- | Try an atomic transaction and,
--   if exceptions specified by @'isRefreshingError'@ has been raised,
--   refreshes the environment and redo the entire transaction.
--
--   Since 0.1.0.0
atomic :: (MonadIO m, MonadCatch m) => RefreshT s m a -> RefreshT s m a
atomic (RefreshT act) = RefreshT $ view isRefreshingError >>= loop
  where
    loop chk =
      catchIf
      chk act $ const $ runRefreshT_ refresh >> loop chk

-- | @'atomicLift' = 'atomic' . 'lift'@.
--
--   Since 0.1.0.0
atomicLift :: (MonadIO m, MonadCatch m) => m a -> RefreshT s m a
atomicLift = atomic . lift

-- | @'atomicLiftIO' = 'atomic' . 'liftIO'@.
--
--   Since 0.1.0.0
atomicLiftIO :: (MonadIO m, MonadCatch m) => IO a -> RefreshT s m a
atomicLiftIO = atomic . liftIO

-- | @'atomicLift'@ composed with @'Control.Monad.Reader.ask'@.
--
--   Since 0.1.0.0
withEnv :: (MonadIO m, MonadCatch m) => (s -> m a) -> RefreshT s m a
withEnv act = atomic $ lift . act =<< ask

-- | Excecute environmental computation and returns the result with the final environment.
--
--   Since 0.1.0.0
runRefreshT :: MonadCatch m => RefreshSetting s m -> s -> RefreshT s m a -> m (a, s)
runRefreshT st s act = do
  (a, s', _) <- runRWST (runRefreshT_ act) st (Localed id s)
  return (a, original s')

-- | Excecute environmental computation and returns the result, discarding the final environment.
--
--   Since 0.1.0.0
evalRefreshT :: MonadCatch m => RefreshSetting s m -> s -> RefreshT s m a -> m a
evalRefreshT st s act = fst <$> evalRWST (runRefreshT_ act) st (Localed id s)

-- | N.B. The refreshed result took place inside @'local'@
--   will be reflected outside.
--
--   Since 0.1.0.0
instance MonadIO m => MonadReader s (RefreshT s m) where
  local f (RefreshT act) = RefreshT $ do
    old <- gets modifier
    modify $ \ls -> ls { modifier = f . old }
    a <- act
    modify (\ls -> ls {modifier = old})
    return a
  ask = RefreshT $ do
    test <- view shouldRefresh
    goRefl <- lift . test =<< gets runLocaled
    if goRefl
      then do
      st <- ask
      liftIO $ threadDelay (st ^. refreshDelay)
      s' <- lift . (st ^. refresher) =<< gets original
      modify $ \ls -> ls { original = s' }
      f <- gets modifier
      return $! f s'
      else gets runLocaled

-- | Since 0.1.1.0
instance MonadThrow m => MonadThrow (RefreshT s m) where
  throwM = RefreshT . throwM
  {-# INLINE throwM #-}

-- | N.B. When exception is @'catch'@ed, no resource refreshment will be occured.
--        This allows users a flexible control on refreshment timing.
instance MonadCatch m => MonadCatch (RefreshT s m) where
  catch (RefreshT a) h = RefreshT $ catch a (runRefreshT_ . h)
  {-# INLINE catch #-}

-- | Forces environmental refreshment, regardless of @'shouldRefresh'@ condition.
--
--   Since 0.1.0.0
refresh :: MonadIO m => RefreshT s m ()
refresh = RefreshT $ do
  st <- ask
  liftIO $ threadDelay (st ^. refreshDelay)
  s' <- lift . (st ^. refresher) =<< gets original
  modify $ \ls -> ls { original = s' }
