{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Web.ReqCatcher
Description : A tiny tool to catch HTTP redirects from the browser
Copyright   : (c) 2016- hiratara
License     : BSD-style
Maintainer  : hiratara@cpan.org
Stability   : experimental

Web.ReqCatcher starts a local HTTP server which handles just one request and
returns that request to the client program. It is useful for CLI program to
capture HTTP redirects from outer WEB services by using browser.

@
  import Web.Authenticate.OAuth

  oauth :: OAuth
  manager :: Manager

  main :: IO ()
  main = do
    c <- newCatcher
    let url = pack (catcherUrl c)
        oauth' = oauth {oauthCallback = Just url}

    credential <- getTemporaryCredential oauth' manager
    putStrLn $ "Access to:\\n" ++ (authorizeUrl oauth credential)
    req <- catchRedirect c

    let (Just (Just verifier)) = lookup "oauth_verifier" (queryString req)
    ...
@
-}
module Web.ReqCatcher
       ( Catcher (catcherUrl)
       , newCatcher
       , newCatcherWithPort
       , catchRedirect
       ) where
import qualified Control.Concurrent as CONC
import qualified Control.Exception as EX
import qualified Network.HTTP.Types as HTTP
import qualified Network.Socket as NW
import qualified Network.Wai as WAI
import qualified Network.Wai.Handler.Warp as WARP
import qualified Data.Text.Lazy as LTXT
import qualified Data.Text.Lazy.Encoding as LTXT

-- | Catcher holds the HTTP server instance and wait for a request.
data Catcher = Catcher
  { catcherUrl :: String -- ^ Target URL of this Catcher
  , catcherWarpThread :: CONC.ThreadId
  , catcherSocket :: NW.Socket
  , catcherCought :: CONC.MVar WAI.Request
  }

-- | Creates the new Catcher instance.
newCatcher :: IO Catcher
newCatcher = newCatcherWithPort =<< pickPort

-- | Creates the new Catcher instance with the specific port.
newCatcherWithPort :: WARP.Port -> IO Catcher
newCatcherWithPort port = do
  mvar <- CONC.newEmptyMVar
  mvarSocket <- CONC.newEmptyMVar

  let set = WARP.setOnException (\_ _ -> return ())
          . WARP.setPort port
          $ WARP.defaultSettings
  tid <- CONC.forkIO (httpWorker mvarSocket set (newCatchApp mvar))
  socket <- CONC.takeMVar mvarSocket

  return $ Catcher (buildURL port) tid socket mvar

httpWorker :: CONC.MVar NW.Socket -> WARP.Settings -> WAI.Application -> IO ()
httpWorker mvar set app =
  EX.bracket
    (NW.socket NW.AF_INET NW.Stream NW.defaultProtocol)
    NW.close
    (\socket -> do
        -- TODO: set Close-On-Exec to socket
        NW.setSocketOption socket NW.ReuseAddr 1
        let addr = NW.SockAddrInet (toEnum $ WARP.getPort set) 0
        NW.bind socket addr
        NW.listen socket 1 -- Handle Just 1 connection
        CONC.putMVar mvar socket
        WARP.runSettingsSocket set socket app
        return ())

-- | Returns the HTTP request cought by Catcher.
--   This function blocks until Catcher catches some requests.
catchRedirect :: Catcher -> IO WAI.Request
catchRedirect catcher = do
  req <- CONC.takeMVar (catcherCought catcher)
  NW.close (catcherSocket catcher)
  return req

pickPort :: IO WARP.Port
pickPort =
  EX.bracket
    (NW.socket NW.AF_INET NW.Stream NW.defaultProtocol)
    NW.close
    (\sock -> do
        NW.setSocketOption sock NW.ReuseAddr 1
        NW.bind sock (NW.SockAddrInet 0 0)
        port <- NW.socketPort sock
        NW.close sock
        return (fromEnum port))

buildURL :: WARP.Port -> String
buildURL port = "http://localhost:" ++ show port

newCatchApp :: CONC.MVar WAI.Request -> WAI.Application
newCatchApp mvar req respond = do
  CONC.putMVar mvar req
  respond $ WAI.responseLBS
    HTTP.status200
    [("Content-Type", "text/plain")]
    (LTXT.encodeUtf8 $ LTXT.pack $ show req)
