{-# LANGUAGE OverloadedStrings,ScopedTypeVariables #-}
module Language.Haskell.Reload.Build where

import Language.Haskell.Reload.FileBrowser
import Language.Haskell.Reload.Project

import Control.Concurrent.MVar
import Control.Exception.Base
import Data.Aeson
import Data.Maybe
import Language.Haskell.Ghcid
import Control.Monad
import Control.Concurrent
import System.FilePath
import System.Directory
import Data.IORef
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.List (isPrefixOf)

import System.Process
import GHC.IO.Handle
import System.IO.Error

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
              else "cabal repl " ++ (if null component then "" else " " ++ component)

replTarget :: FilePath -> ReplTargetGroup -> IO (ReplTarget,[Load])
replTarget root grp = do
  let nm=rtgName grp
  cmd <- replCommand root (projectName root) nm
  -- putStrLn cmd
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
  catch ( do
    (tgts,loads) <- case mcabal of
              Nothing -> return ([],[])
              Just cf -> do
                grps <- readTargetGroups cf
                unzip <$> mapM (replTarget root) grps
    _ <- tryPutMVar buildResult $ loadsToValue root (ordNub $ concat loads)
    return tgts
    )
    (\(e::GhciError) -> do
      putStrLn $ "startSessions error:" ++ (show e)
      return [])

rebuild :: BuildState -> FilePath -> IO ()
rebuild bs@(BuildState root buildResult mghci int _) path = void $ forkIO $ do
  ghci <- readIORef mghci
  if null ghci || shouldRestart path
    then
      restartBuild bs
    else do
      let sess=filter (matchGroup path . rtGroup) ghci
      --putStrLn $ "sess:" ++ (show $ length sess)
      loads <- withMVar int $ \_-> do
        --putStrLn "got int"
        catch (do
          mapM_ (interrupt . rtGhci) sess
          loads <- mapM (reload . rtGhci) sess
          --putStrLn $ "put int:" ++ (show loads)
          return loads
          )
          (\(e::GhciError) -> do
            putStrLn $ "rebuild error:" ++ (show e)
            return [])
      when (not $ null loads) $ do
        let cloads = ordNub $ concat loads
        ok1 <- tryPutMVar buildResult $ loadsToValue root cloads
        when (not ok1) $ do
          void $ tryTakeMVar buildResult
          void $ tryPutMVar buildResult $ loadsToValue root cloads

restartBuild :: BuildState -> IO ()
restartBuild (BuildState root buildResult mghci _ _) = do
  ghci <- readIORef mghci
  putStrLn $ "Rebuilding:"++(show $ length ghci)
  _ <- mapM (quit . rtGhci) ghci
  tgts <- startSessions root buildResult
  writeIORef mghci tgts

shouldRestart :: FilePath -> Bool
shouldRestart fp = let
  f = takeFileName fp
  in (f == stackFile || takeExtension f == ".cabal")

loadsToValue :: FilePath -> [Load] -> Value
loadsToValue root loads = object ["loads" .= map (loadToValue root) loads]

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

launch :: T.Text -> String -> BuildState -> IO ()
launch name command (BuildState root buildResult _ _ _)  = void $ forkIO $ do
  void $ tryPutMVar buildResult (object ["process" .= name])
  resolvedCommand <- if ("<tool>" `isPrefixOf` command)
    then do
      hs <- hasStack root
      let tc = if hs then "stack" else "cabal"
      return $ tc ++ drop 6 command
    else return command
  putStrLn resolvedCommand
  let cp = (shell resolvedCommand) {cwd = Just root, std_out = CreatePipe, std_err = CreatePipe}
  (_,Just out,Just err,_) <- createProcess cp
  processHandle out "out"
  processHandle err "err"
  where
    processHandle :: Handle -> String -> IO ()
    processHandle h str = void $ forkIO $  do
      hSetBuffering h LineBuffering
      hSetBinaryMode h False
      void $ tryIOError $ forever $ do
        line <- hGetLine h
        putMVar buildResult (object ["process" .= name, "line" .= line, "stream" .= str])
      when ("out" == str) $ putMVar buildResult (object ["process" .= name, "line" .= ("<end>"::T.Text), "stream" .= str])
      
      
withModule :: BuildState -> FilePath -> (Ghci -> IO a) -> IO (Maybe a)
withModule bs fp f = do
  ghci <- readIORef (bsGhci bs)
  let mmrt = msum $ map (moduleNameGroup fp) ghci
  case mmrt of
    Just (m,rt) -> do
      let act = bsAction bs
      withMVar act $ \_->
        void $ exec (rtGhci rt) $ ":module *" ++ m
      Just <$> f (rtGhci rt)
    Nothing -> return Nothing
  where
    moduleNameGroup fp1 rt = let
      mg = moduleName fp1 $ rtGroup rt
      in case mg of
        Nothing -> Nothing
        Just m -> Just (m,rt)
    
info :: BuildState -> FilePath -> String -> IO [String]
info bs fp s = do
  ms <- withModule bs fp $ \ghci -> do
    let act = bsAction bs
    withMVar act $ \_->
        exec ghci $ ":i " ++ s
  return $ fromMaybe [] ms
