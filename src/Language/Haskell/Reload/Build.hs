{-# LANGUAGE OverloadedStrings #-}
module Language.Haskell.Reload.Build where

import Language.Haskell.Reload.FileBrowser
import Language.Haskell.Reload.Project

import Control.Concurrent.MVar
import Data.Aeson
import Language.Haskell.Ghcid
import Control.Monad
import Control.Concurrent
import System.FilePath
import System.Directory
import Data.IORef
import qualified Data.Set as Set

data BuildState = BuildState
  { bsRoot :: FilePath
  , bsBuildResult :: MVar Value
  , bsGhci :: IORef [ReplTarget]
  , bsInterrupt :: MVar ()
  , bsAction :: MVar ()
  }

data ReplTarget = ReplTarget
  { rtGroup :: ReplTargetGroup
  , rtGhci :: Ghci
  }

stackFile :: FilePath
stackFile = "stack.yaml"

hasStack :: FilePath -> IO Bool
hasStack root = do
  let stackF  = root </> stackFile
  doesFileExist stackF

replCommand :: FilePath -> String -> String -> IO String
replCommand root project component = do
  isStack <- hasStack root
  return $ if isStack
              then "stack repl " ++ (if null component then "" else project ++ ":" ++ component)
              else "cabal repl" ++ (if null component then "" else " " ++ component)

replTarget :: FilePath -> ReplTargetGroup -> IO (ReplTarget,[Load])
replTarget root grp = do
  let nm=rtgName grp
  cmd <- replCommand root (projectName root) nm
  putStrLn cmd
  (ghci,load) <- startGhci cmd (Just root) (\_ l -> putStrLn l)
  return (ReplTarget grp ghci,load)

startBuild :: FilePath -> MVar Value -> Bool -> IO BuildState
startBuild root buildResult withRepl= do
  tgts <- if withRepl then startSessions root buildResult else return []
  mghci <- newIORef tgts
  int <- newMVar ()
  act <- newMVar ()
  return $ BuildState root buildResult mghci int act

startSessions :: FilePath -> MVar Value -> IO [ReplTarget]
startSessions root buildResult = do
  mcabal <- cabalFileInFolder root
  (tgts,loads) <- case mcabal of
            Nothing -> return ([],[])
            Just cf -> do
              grps <- readTargetGroups cf
              unzip <$> mapM (replTarget root) grps
  _ <- tryPutMVar buildResult $ loadsToValue root (ordNub $ concat loads)
  return tgts

rebuild :: BuildState -> FilePath -> IO ()
rebuild bs@(BuildState root buildResult mghci int _) path = void $ forkIO $ do
  ghci <- readIORef mghci
  if shouldRestart path
    then
      restartBuild bs
    else do
      let sess=filter (matchGroup path . rtGroup) ghci
      --putStrLn $ "sess:" ++ (show $ length sess)
      loads <- withMVar int $ \_-> do
        --putStrLn "got int"
        mapM_ (interrupt . rtGhci) sess
        loads <- mapM (reload . rtGhci) sess
        --putStrLn $ "put int:" ++ (show loads)
        return loads
      when (not $ null loads) $ do
        let cloads = ordNub $ concat loads
        ok1 <- tryPutMVar buildResult $ loadsToValue root cloads
        when (not ok1) $ do
          _ <- tryTakeMVar buildResult
          void $ tryPutMVar buildResult $ loadsToValue root cloads

restartBuild :: BuildState -> IO ()
restartBuild (BuildState root buildResult mghci _ _) = do
  ghci <- readIORef mghci
  _ <- mapM (quit . rtGhci) ghci
  tgts <- startSessions root buildResult
  writeIORef mghci tgts

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



ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs
