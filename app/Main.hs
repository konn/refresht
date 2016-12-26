{-# LANGUAGE DeriveAnyClass #-}
module Main where
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.Refresh
import Data.Default
import Data.IORef
import Data.Typeable

data RefreshNow = RefreshNow
                deriving (Eq, Show, Typeable, Exception)

conf :: RefreshSetting Int IO
conf = def & isRefreshingError .~ ((== Just RefreshNow) . fromException)
           & shouldRefresh .~ const (return False)
           & refresher .~ (return . succ)
           & refreshDelay .~ (10 ^ 5)

main :: IO ()
main = do
  ref <- newIORef True
  let conf' = conf & refresher .~ \a -> do
        writeIORef ref False
        return (succ a)
  print =<< runRefreshT conf 0 (return ())
  print =<< runRefreshT conf 0 (atomicLift $ return ())
  print =<< runRefreshT conf' 0 (atomicLift $ do (throw RefreshNow); return ())
