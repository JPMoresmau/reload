module Language.Haskell.Reload.Project where

import Data.IORef
import Data.List (isPrefixOf)
import Data.Maybe

import Distribution.PackageDescription hiding (Library, Executable, TestSuite, Benchmark)
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration
import Distribution.Verbosity

import System.Directory
import System.FilePath

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
  let ltgts= map (Target Library "" . hsSourceDirs . libBuildInfo) $ maybeToList $ library pd
  let etgts= map (\e->Target Executable (exeName e)  (hsSourceDirs $ buildInfo e)) $ executables pd
  let ttgts= map (\t->Target TestSuite (testName t) (hsSourceDirs $ testBuildInfo t)) $ testSuites pd
  let btgts= map (\b->Target Benchmark (benchmarkName b) (hsSourceDirs $ benchmarkBuildInfo b)) $ benchmarks pd
  return $ ltgts ++ etgts ++ ttgts ++ btgts


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

projectName :: FilePath -> String
projectName root = takeFileName $ dropTrailingPathSeparator root

cabalFileInFolder :: FilePath -> IO (Maybe FilePath)
cabalFileInFolder root = do
  let dir = projectName root
  let f =root </> (addExtension dir "cabal")
  ex <- doesFileExist f
  if ex
    then return $ Just f
    else do
      fs <- getDirectoryContents root
      let visible = filter (not . ("." `isPrefixOf`) . takeFileName) fs
      let cabals = map (\cf->root</>cf) $ filter (("cabal" ==) . takeExtension) visible
      return $ listToMaybe cabals


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
