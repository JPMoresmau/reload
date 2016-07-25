{-# LANGUAGE OverloadedStrings,ScopedTypeVariables #-}
-- | Build and GHCi
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

-- | Build information
data BuildState = BuildState
  { bsRoot :: FilePath -- ^ project root
  , bsBuildResult :: MVar Value -- ^ JSON to indicate the output of a build
  , bsGhci :: IORef [ReplTarget] -- ^ GHCi session by target
  , bsInterrupt :: MVar () -- ^ Semaphore for actions that may interrupt others, like build
  , bsAction :: MVar () -- ^ Semaphore for actions that can be interrupted
  }

-- | Target and GHCi
data ReplTarget = ReplTarget
  { rtGroup :: ReplTargetGroup
  , rtGhci :: Ghci
  }

-- | Stack file name
stackFile :: FilePath
stackFile = "stack.yaml"


-- | Do we have a stack file in the given folder?
hasStack :: FilePath -> IO Bool
hasStack root = do
  let stackF  = root </> stackFile
  doesFileExist stackF

-- | Build the REPL Command
replCommand 
  :: FilePath -- ^ Project root
  -> String   -- ^ Project name
  -> String   -- ^ Component name
  -> IO String
replCommand root project component = do
  isStack <- hasStack root
  return $ if isStack
              then "stack repl " ++ (if null component then "" else project ++ ":" ++ component)
              else "cabal repl " ++ (if null component then "" else " " ++ component)

-- | Start a GHCi session
replTarget 
  :: FilePath -- ^ Project root
  -> ReplTargetGroup -- ^ Target group
  -> IO (ReplTarget,[Load])
replTarget root grp = do
  let nm=rtgName grp
  cmd <- replCommand root (projectName root) nm
  -- putStrLn cmd
  (ghci,load) <- startGhci cmd (Just root) (\_ l -> putStrLn l)
  return (ReplTarget grp ghci,load)

-- | Start all GHCi sessions and build state
startBuild 
  :: FilePath -- ^ project root
  -> MVar Value  -- ^ build output variable
  -> Bool  -- ^ should we really start a REPL?
  -> IO BuildState
startBuild root buildResult withRepl= do
  tgts <- if withRepl then startSessions root buildResult else return []
  mghci <- newIORef tgts
  int <- newMVar ()
  act <- newMVar ()
  return $ BuildState root buildResult mghci int act

-- | Start GHCi sessions
startSessions 
  :: FilePath  -- ^ project root
  -> MVar Value -- ^ build output variable
  -> IO [ReplTarget]
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

-- | Reload when a file changes
rebuild 
  :: BuildState -- ^ the state
  -> FilePath -- ^ the changed file
  -> IO ()
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

-- | Restart the sessio
restartBuild :: BuildState -> IO ()
restartBuild (BuildState root buildResult mghci _ _) = do
  ghci <- readIORef mghci
  -- putStrLn $ "Rebuilding:"++(show $ length ghci)
  _ <- mapM (quit . rtGhci) ghci
  tgts <- startSessions root buildResult
  writeIORef mghci tgts

-- | Should we restart the session (when the stack or cabal files have changed)
shouldRestart :: FilePath -> Bool
shouldRestart fp = let
  f = takeFileName fp
  in (f == stackFile || takeExtension f == ".cabal")

-- | Convert GHCid load values into JSON
loadsToValue :: FilePath -> [Load] -> Value
loadsToValue root loads = object ["loads" .= map (loadToValue root) loads]

-- | Convert one GHCid load value into JSON
loadToValue :: FilePath -> Load -> Value
loadToValue root (Loading modu file)=object ["module".=modu,"file".=(relative root file),"mime".=getMIMEText file]
loadToValue root (Message sev file (ln,col) msg)=object ["severity".=(show sev),"file".=(relative root file),"line".=ln,"column".=col,"message".=msg,"mime".=getMIMEText file]

-- | Make a path relative (sometimes paths are given absolute by GHCi)
relative :: FilePath -> FilePath -> FilePath
relative root fp
  | isRelative fp = fp
  | otherwise = makeRelative root fp


-- | Infamous nub for Ord instances
ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty l
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs

-- | Launch a command
launch 
  :: T.Text -- ^ Name the command is referenced by
  -> String -- ^ Actual command
  -> BuildState -- ^ Our state
  -> IO ()
launch name command (BuildState root buildResult _ _ _)  = void $ forkIO $ do
  void $ tryPutMVar buildResult (object ["process" .= name])
  resolvedCommand <- if ("<tool>" `isPrefixOf` command)
    then do
      hs <- hasStack root
      let tc = if hs then "stack" else "cabal"
      return $ tc ++ drop 6 command
    else return command
  runExec resolvedCommand root
    (\str line -> putMVar buildResult (object ["process" .= name, "line" .= line, "stream" .= str]))
    (\str -> when ("out" == str) $ putMVar buildResult (object ["process" .= name, "line" .= ("<end>"::T.Text), "stream" .= str]))

-- | Run an executable
runExec 
  :: String -- ^ Full command
  -> FilePath -- ^ Directory to run in
  -> (String -> String -> IO())  -- ^ Handler on each line
  -> (String -> IO()) -- ^ Handler on end
  -> IO()
runExec resolvedCommand root onLine onEnd = do
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
        onLine str line
      onEnd str
      
-- | Ensures GHCi is in the module given by the file path
withModule 
  :: BuildState -- ^ our state
  -> FilePath -- ^ The file path
  -> (Ghci -> IO a) -- ^ What to do in GHCi once the module is set
  -> IO (Maybe a)
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
    
-- | Run the :info command on the given word
info :: BuildState -> FilePath -> String -> IO [String]
info bs fp s = do
  ms <- withModule bs fp $ \ghci -> do
    let act = bsAction bs
    withMVar act $ \_->
        exec ghci $ ":i " ++ s
  return $ fromMaybe [] ms

-- | Run the :complete command on the given prefix
-- we unquote the string literals complete returns
complete :: BuildState -> FilePath -> String -> IO [String]
complete bs fp s = do 
  ms <- withModule bs fp $ \ghci -> do
    let act = bsAction bs
    withMVar act $ \_->
        exec ghci $ ":complete repl \"" ++ s ++ "\""
  return $ maybe [] (map unquote . filter (not . null) . drop 1) ms
  where
    unquote = tail . init -- garanteed not empty