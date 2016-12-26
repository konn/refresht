RefreshT -- Environment Monad with automatic resource refreshment
==================================================================

Overview
--------
`refresht` package provides similar interface as `ReaderT`-monad,
but it comes with *automatic refreshment* mechanism.
In other words, the `RefreshT` monad transformer provides the
general way to maintain the *freshness* of the external environment,
with respoec to the specified condition or exceptions.

The typical usage is to communicate with the server which requires
periodic refreshment of access tokens, such as Google API.

Usage
-----
The following pseudo-code illustrates the typical usage:

```haskell
import Control.Monad.Refresh

import Network.Wreq               (getWith, defaults)
import Control.Lens               ((&), (.~), (^.))
import Data.Time                  (addUTCTime, getCurrentTime, UTCTime)
import Data.ByetString.Lazy.Char8 (unpack)
import Control.Exception          (fromException)

data User = User { expiration  :: UTCTime
                 , accessToken :: String
                 }

main :: IO ()
main = do
  time <- getCurrentTime
  evalRefreshT conf (User (3600 `addUTCTime` time) "initialtoken") $ do
    rsc <- withEnv $ \User{..} ->
      getWith (defaults & auth .~ oauth2Bearer accessToken)
              "https://example.com/api/resource"
    putStrLn $ print rsc

conf :: RefreshSetting User IO
conf = defaultRefreshSetting
     & refresher         .~ update
     & shouldRefresh     .~ checkExpr
     & isRefreshingError .~ isRefreshing
  where
    shouldRefresh usr = do -- checks expiration
      now <- getCurrentTime
      return $ now <= expiration usr
          
    update usr = do
      -- Refreshed token for the service
      bdy <- getWith
        (defaults & param "token" .~ [accessToken usr])
        "https://example.com/api/refresh_token"
      let token = unpack $ bdy ^. responseBody
          usr'  = usr { accessToken = token
                      , expiration = ...
                      }

      -- Save updates in local file, or db.
      writeFile "database" (show usr')
      return usr'

    -- 401 Unauthorized exception should cause refreshment
    isRefreshing e =
      case fromException e of
        Just Error401 -> True
        _ -> False
```
