{-# LANGUAGE OverloadedStrings, QuasiQuotes, ScopedTypeVariables #-}
module Language.Haskell.Reload (runApp, app) where

import Language.Haskell.Reload.Build
import Language.Haskell.Reload.FileBrowser

import           Data.Aeson (Value(..),encode)
import           Network.Wai
import           Network.Wai.Handler.Warp
import Network.Wai.Middleware.Static
import Network.HTTP.Types.Status
import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdout)
import System.Directory
import Control.Monad
import Control.Monad.IO.Class
import System.FilePath
import qualified Data.ByteString.Lazy as B
import Data.Text.Lazy (fromStrict)
import Data.List (isInfixOf)

import Network.Wai.Handler.WebSockets
import Network.WebSockets
import Control.Concurrent
import Control.Exception

app' :: BuildState -> ScottyM ()
app' buildState = do
  middleware $ staticPolicy (addBase "web")
  middleware $ logStdout
  get "/" $ file "web/index.html"
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
      cnt <- liftIO $ do
        B.readFile ((bsRoot buildState) </> path)
      setHeader "Content-Type" $ fromStrict $ getMIMEText path
      raw cnt
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

checkPath :: FilePath -> ActionM () -> ActionM ()
checkPath path f = do
  if (".." `isInfixOf` path || isAbsolute path)
    then do
      status notFound404
      json Null
    else f

app :: Bool -> IO Application
app withRepl = do
  root <- getCurrentDirectory
  buildResult <- newEmptyMVar
  buildState<- startBuild root buildResult withRepl
  putStrLn $ "Ready!"
  sco <- scottyApp $ app' buildState
  return $ websocketsOr defaultConnectionOptions (wsApp buildResult) sco

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
    let setts = setPort port defaultSettings
    putStrLn $ "Preparing to serve on http://localhost:" ++(show port)++"..."
    runSettings setts =<< app True
