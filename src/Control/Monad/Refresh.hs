{-# LANGUAGE DeriveFunctor, FlexibleContexts, FlexibleInstances, GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, KindSignatures                #-}
{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables                #-}
{-# LANGUAGE StandaloneDeriving, TemplateHaskell                       #-}
module Control.Monad.Refresh
       ( -- * @'RefreshT'@ type and settings
         RefreshT, runRefreshT, evalRefreshT
       , RefreshSetting
       , defaultRefreshSetting
       , refresher
         -- | Environment refreshing function
       , refreshDelay
         -- | Delay before environmental refreshment (default: @100ms@).
       , shouldRefresh
         -- | Condition to determine if environment should be refreshed (default: @const True@).
       , isRefreshingError
         -- | If this exception should occur an envionment refreshment? (default: refresh for any exception).
       , atomic
       , atomicLift
       , withEnv
       , refresh
       ) where
import Control.Concurrent  (threadDelay)
import Control.Exception   (SomeException (..))
import Control.Lens        (makeLenses, view, (^.))
import Control.Monad.Catch (MonadCatch (..), catchIf)
import Control.Monad.RWS   (MonadTrans (..), RWST (..), ask, evalRWST, get,
                            gets)
import Control.Monad.RWS   (MonadIO (liftIO), MonadReader (..), modify, runRWST)
import Data.Default        (Default (..))
import Data.Typeable       (Typeable)

data RefreshSetting s m =
  RefreshSetting { _refresher         :: s -> m s
                 , _refreshDelay      :: Int
                 , _isRefreshingError :: SomeException -> Bool
                 , _shouldRefresh     :: s -> m Bool
                 }
  deriving (Typeable)

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
newtype RefreshT s m a =
  RefreshT { runRefreshT_ :: RWST (RefreshSetting s m) () (Localed s) m a }
  deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (RefreshT s) where
  lift = RefreshT .lift

-- | Try an atomic transaction and,
--   if exceptions specified by @'isRefreshingError'@ has been raised,
--   refreshes the environment and redo the entire transaction.
atomic :: (MonadIO m, MonadCatch m) => RefreshT s m a -> RefreshT s m a
atomic (RefreshT act) = RefreshT $ view isRefreshingError >>= loop
  where
    loop chk =
      catchIf
      chk act $ const $ runRefreshT_ refresh >> loop chk

-- | @'atomicLift' = 'atomic' . 'lift'@.
atomicLift :: (MonadIO m, MonadCatch m) => m a -> RefreshT s m a
atomicLift = atomic . lift

-- | @'atomicLift'@ composed with @'Control.Monad.Reader.ask'@.
withEnv :: (MonadIO m, MonadCatch m) => (s -> m a) -> RefreshT s m a
withEnv act = atomic $ lift . act =<< ask

runRefreshT :: MonadCatch m => RefreshSetting s m -> s -> RefreshT s m a -> m (a, s)
runRefreshT st s act = do
  (a, s', _) <- runRWST (runRefreshT_ act) st (Localed id s)
  return (a, original s')

evalRefreshT :: MonadCatch m => RefreshSetting s m -> s -> RefreshT s m a -> m a
evalRefreshT st s act = fst <$> evalRWST (runRefreshT_ act) st (Localed id s)

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

-- | Forces environmental refreshment, regardless of @'shouldRefresh'@ condition.
refresh :: MonadIO m => RefreshT s m ()
refresh = RefreshT $ do
  st <- ask
  liftIO $ threadDelay (st ^. refreshDelay)
  s' <- lift . (st ^. refresher) =<< gets original
  modify $ \ls -> ls { original = s' }
