{-# LANGUAGE OverloadedStrings, QuasiQuotes, ScopedTypeVariables,CPP #-}
-- | The definition of the Scotty application, and lauching code
-- the launch code is borrowed from <https://github.com/yesodweb/wai/blob/master/wai-handler-launch/Network/Wai/Handler/Launch.hs>
-- but we couldn't reuse the library because it injects code to every HTML page, so here it's going to inject the code to HTML code we want to edit
-- my-app.html does the pinging from the client, no need to inject JS code
module Language.Haskell.Reload (runApp, app) where

import Language.Haskell.Reload.Build
import Language.Haskell.Reload.FileBrowser

import           Data.Aeson (Value(..),encode)
import           Network.Wai
import           Network.Wai.Handler.Warp
import Network.Wai.Middleware.Static
import Network.HTTP.Types.Status
import Web.Scotty
-- import Network.Wai.Middleware.RequestLogger (logStdout)
import System.Directory
import Control.Monad
import Control.Monad.IO.Class
import System.FilePath
import qualified Data.ByteString.Lazy as B
import Data.Text.Lazy (fromStrict,unpack)
import qualified Data.Text.Lazy.Encoding as T
import Data.List (isInfixOf)

import Network.Wai.Handler.WebSockets
import Network.WebSockets
import Control.Concurrent
import Control.Concurrent.Async (race)
import Control.Exception
import Data.IORef

#if WINDOWS
import Foreign
import Foreign.C.String
#else
import System.Process (rawSystem)
#endif

scottyDef :: IORef Bool -> BuildState -> ScottyM ()
scottyDef active buildState = do
  middleware $ staticPolicy (addBase "web")
  -- middleware $ logStdout
  get "/" $ file "web/index.html"
  get (regex "^/ping$") $ do
    liftIO $ writeIORef active True
    status ok200
    json Null
  get (regex "^/files(.*)$") $ do
    path <- param "1"
    let norm = case path of
                ('/':r)->r
                _ -> path
    checkPath norm $ do
      fss <- liftIO $ do
        listFiles (bsRoot buildState) norm
      json fss
  put (regex "^/files/(.*)$") $ do
    path <- param "1"
    checkPath path $ do
      ex <- liftIO $ do
              let full = (bsRoot buildState) </> path
              ex <- doesDirectoryExist full
              createDirectoryIfMissing True full
              return ex
      if ex
        then status noContent204
        else status created201
      json Null
  delete (regex "^/files/(.*)$") $ do
      path <- param "1"
      checkPath path $ do
        ex <- liftIO $ do
                let full = (bsRoot buildState) </> path
                ex <- doesDirectoryExist full
                when ex $ removeDirectoryRecursive full
                return ex
        if ex
          then status ok200
          else status noContent204
        json Null
  get (regex "^/file/(.*)$") $ do
    path <- param "1"
    checkPath path $ do
      let fp = (bsRoot buildState) </> path
      ex <- liftIO $ doesFileExist fp
      if ex
          then do
            cnt <- liftIO $ B.readFile fp
            setHeader "Content-Type" $ fromStrict $ getMIMEText path
            raw cnt
          else status notFound404
  put (regex "^/file/(.*)$") $ do
    path <- param "1"
    checkPath path $ do
      b <- body
      ex <- liftIO $ do
              let p = (bsRoot buildState) </> path
              exd <- doesDirectoryExist p
              if exd
                then
                  return Nothing
                else do
                  ex <- doesFileExist p
                  when (not ex) $ do
                    createDirectoryIfMissing True $ takeDirectory p
                  B.writeFile p b
                  rebuild buildState path
                  return $ Just ex
      case ex of
        Just True -> status ok200
        Just False -> status created201
        Nothing -> status forbidden403
      json Null
  delete (regex "^/file/(.*)$") $ do
    path <- param "1"
    checkPath path $ do
      ex <- liftIO $ do
              let p = (bsRoot buildState) </> path
              ex <- doesFileExist p
              when ex $ do
                removeFile p
                rebuild buildState path
              return ex
      if ex
        then status ok200
        else status noContent204
      json Null
  post (regex "^/launch/(.*)$") $ do
    name <- param "1"
    bs <- body
    let s = unpack $ T.decodeUtf8 bs
    liftIO $ launch name s buildState
    json Null

checkPath :: FilePath -> ActionM () -> ActionM ()
checkPath path f = do
  if (".." `isInfixOf` path || isAbsolute path)
    then do
      status notFound404
      json Null
    else f

fullApp :: IORef Bool -> Bool -> IO Application
fullApp active withRepl = do
  root <- getCurrentDirectory
  buildResult <- newEmptyMVar
  buildState<- startBuild root buildResult withRepl
  putStrLn $ "Ready!"
  sco <- scottyApp $ scottyDef active buildState
  return $ websocketsOr defaultConnectionOptions (wsApp buildResult) sco

app :: Bool -> IO Application
app withRepl = do
  active <- newIORef True
  fullApp active withRepl

wsApp :: (MVar Value) -> ServerApp
wsApp buildResult pending_conn = do
    conn <- acceptRequest pending_conn
    forkPingThread conn 30
    sendBuild conn
  where
    sendBuild conn = do
      v <- takeMVar buildResult
      sendTextData conn (encode v)
      catch (do
        b::B.ByteString <- receiveData conn
        when ("\"OK\""== b) $ sendBuild conn
       ) (\(e::ConnectionException) -> do
        void $ tryPutMVar buildResult v
        throw e
        )

runApp :: Int -> IO ()
runApp port = do --scotty port app
    ready <- newEmptyMVar
    let setts =
                setBeforeMainLoop (putMVar ready ()) $
                setPort port defaultSettings
    putStrLn $ "Preparing to serve on http://localhost:" ++(show port)++"..."
    active <- newIORef True
    fmap (either id id) $ race
          -- serve app, keep updating the activity flag
          (runSettings setts =<< fullApp active True)
          -- wait for server startup, launch browser, poll until server idle
          (takeMVar ready >> launchBrowser port "" >> loop active)

loop :: IORef Bool -> IO ()
loop active = do
  let seconds = 120
  threadDelay $ 1000000 * seconds
  b <- readIORef active
  if b
      then writeIORef active False >> loop active
      else return ()


#if WINDOWS
foreign import ccall "launch"
    launch' :: Int -> CString -> IO ()
#endif

launchBrowser :: Int -> String -> IO ()

#if WINDOWS
launchBrowser port s = withCString s $ launch' port
#else
launchBrowser port s = forkIO (rawSystem
#if MAC
    "open"
#else
    "xdg-open"
#endif
    ["http://127.0.0.1:" ++ show port ++ "/" ++ s] >> return ()) >> return ()
#endif
