{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Reload.Build where

import Language.Haskell.Reload.FileBrowser

import Data.Aeson
import Language.Haskell.Ghcid
import Control.Monad
import Control.Concurrent
import System.FilePath
import System.Directory
import Data.IORef

data BuildState = BuildState
  { bsRoot :: FilePath
  , bsBuildResult :: MVar Value
  , bsGhci :: IORef Ghci
  }

stackFile :: FilePath
stackFile = "stack.yaml"

hasStack :: FilePath -> IO Bool
hasStack root = do
  let stackF  = root </> stackFile
  doesFileExist stackF

replCommand :: FilePath -> IO String
replCommand root = do
  isStack <- hasStack root
  return $ if isStack
              then "stack repl"
              else "cabal repl"

startBuild :: FilePath -> MVar Value -> IO BuildState
startBuild root buildResult = do
  cmd <- replCommand root
  (ghci,load) <- startGhci cmd (Just root) (\_ _ -> return ())
  _ <- tryPutMVar buildResult $ loadsToValue root load
  mghci <- newIORef ghci
  return $ BuildState root buildResult mghci

rebuild :: BuildState -> FilePath -> IO ()
rebuild bs@(BuildState root buildResult mghci) path = void $ forkIO $ do
  ghci <- readIORef mghci
  interrupt ghci
  if shouldRestart path
    then
      restartBuild bs
    else do
      load <- reload ghci
      ok1 <- tryPutMVar buildResult $ loadsToValue root load
      when (not ok1) $ do
        _ <- tryTakeMVar buildResult
        void $ tryPutMVar buildResult $ loadsToValue root load

restartBuild :: BuildState -> IO ()
restartBuild (BuildState root buildResult mghci) = do
  ghci <- readIORef mghci
  quit ghci
  cmd <- replCommand root
  (ghci2,load) <- startGhci cmd (Just root) (\_ _ -> return ())
  _ <- tryPutMVar buildResult $ loadsToValue root load
  writeIORef mghci ghci2

shouldRestart :: FilePath -> Bool
shouldRestart fp = let
  f = takeFileName fp
  in (f == stackFile || takeExtension f == ".cabal")

loadsToValue :: FilePath -> [Load] -> Value
loadsToValue root = toJSON . map (loadToValue root)

loadToValue :: FilePath -> Load -> Value
loadToValue root (Loading modu file)=object ["module".=modu,"file".=(relative root file),"mime".=getMIMEText file]
loadToValue root (Message sev file (ln,col) msg)=object ["severity".=(show sev),"file".=(relative root file),"line".=ln,"column".=col,"message".=msg,"mime".=getMIMEText file]

relative :: FilePath -> FilePath -> FilePath
relative root fp
  | isRelative fp = fp
  | otherwise = makeRelative root fp
