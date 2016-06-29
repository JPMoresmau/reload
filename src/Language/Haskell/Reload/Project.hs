module Language.Haskell.Reload.Project where

import Data.IORef
import Data.List (isPrefixOf)

import Distribution.PackageDescription hiding (Library, Executable, TestSuite, Benchmark)
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration
import Distribution.Verbosity

data TargetType = Library | Executable | TestSuite | Benchmark
  deriving (Read,Show,Eq,Ord,Bounded,Enum)
  
data Target = Target 
  { tType::TargetType
  , tName :: String
  , tSourceDirs :: [FilePath]
  } deriving (Read,Show,Eq,Ord)
  
readTargets :: FilePath -> IO [Target]
readTargets cabalFile = do
  gpd <- readPackageDescription silent cabalFile
  let pd = flattenPackageDescription gpd
  ioTgt <- newIORef []
  withLib pd (addTgt ioTgt Library "" . libBuildInfo)
  withExe pd (\e->addTgt ioTgt Executable (exeName e) (buildInfo e))
  withTest pd (\t->addTgt ioTgt TestSuite (testName t) (testBuildInfo t))
  withBenchmark pd (\b->addTgt ioTgt Benchmark (benchmarkName b) (benchmarkBuildInfo b))
  tgts <- readIORef ioTgt
  return $ reverse tgts
  where 
    addTgt ior tp nm bi = modifyIORef ior (\tgts-> Target tp nm (hsSourceDirs bi) : tgts)
    
    
data ReplTargetGroup = ReplTargetGroup
 { rtgName :: String
 , rtgTargets :: [Target]
 } deriving (Read,Show,Eq,Ord)
 
targetGroups :: [Target] -> [ReplTargetGroup]
targetGroups tgts
  | (t1:t2:rs) <- tgts
    , tType t1 == Library
    , tType t2 == Executable = (ReplTargetGroup "" [t1,t2]:map singleGroup rs)
  | otherwise = map singleGroup tgts
  where
    singleGroup t = ReplTargetGroup (tName t) [t]

readTargetGroups :: FilePath -> IO [ReplTargetGroup]
readTargetGroups cabalFile = targetGroups <$> readTargets cabalFile

matchGroup :: FilePath -> ReplTargetGroup -> Bool
matchGroup fp gr = let
  fps = concatMap (normSourceDirs . tSourceDirs) $ rtgTargets gr
  in any (\r-> r=="" || r `isPrefixOf` fp) fps
  
normSourceDirs :: [FilePath] -> [FilePath]
normSourceDirs [] = [""]
normSourceDirs rs = map norm rs
  where norm "." = ""
        norm "" = ""
        norm r 
          | last r == '/' = r
          | otherwise = r ++ "/"