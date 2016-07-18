{-# LANGUAGE OverloadedStrings #-}
-- | Project handling, mainly using the Cabal file
module Language.Haskell.Reload.Project where

import Control.Monad
import Data.Aeson
import Data.List (isPrefixOf)
import Data.Maybe
import qualified Data.Text as T
import Distribution.PackageDescription hiding (Library, Executable, TestSuite, Benchmark)
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.Configuration
import Distribution.Verbosity

import System.Directory
import System.FilePath

-- | Type of target
data TargetType = Library | Executable | TestSuite | Benchmark
  deriving (Read,Show,Eq,Ord,Bounded,Enum)

-- | Json serialize
instance ToJSON TargetType where
  toJSON = String . T.pack . show

-- | Json hydrate
instance FromJSON TargetType where
  parseJSON (String s) = pure $ read $ T.unpack s
  parseJSON _ = fail "TargetType"

-- | A target (component in a cabal file)
data Target = Target
  { tType :: TargetType -- ^ Type 
  , tName :: String -- ^ Name (empty for Library)
  , tSourceDirs :: [FilePath] -- ^ Source folders
  } deriving (Read,Show,Eq,Ord)

-- | Json serialize
instance ToJSON Target where
  toJSON (Target t n _) = object ["type" .= t, "name" .= n]

-- | Read targets from Cabal file
readTargets :: FilePath -> IO [Target]
readTargets cabalFile = do
  gpd <- readPackageDescription silent cabalFile
  let pd = flattenPackageDescription gpd
  let ltgts= map (Target Library "" . hsSourceDirs . libBuildInfo) $ maybeToList $ library pd
  let etgts= map (\e->Target Executable (exeName e)  (hsSourceDirs $ buildInfo e)) $ executables pd
  let ttgts= map (\t->Target TestSuite (testName t) (hsSourceDirs $ testBuildInfo t)) $ testSuites pd
  let btgts= map (\b->Target Benchmark (benchmarkName b) (hsSourceDirs $ benchmarkBuildInfo b)) $ benchmarks pd
  return $ ltgts ++ etgts ++ ttgts ++ btgts

-- | A group of target that GHCi can handle together
data ReplTargetGroup = ReplTargetGroup
 { rtgName :: String
 , rtgTargets :: [Target]
 } deriving (Read,Show,Eq,Ord)

-- | Json serialize
instance ToJSON ReplTargetGroup where
  toJSON (ReplTargetGroup n tgts) = object ["name" .= n, "targets" .= tgts]

-- | Group targets (the first executable can go with the library)
targetGroups :: [Target] -> [ReplTargetGroup]
targetGroups tgts
  | (t1:t2:rs) <- tgts
    , tType t1 == Library
    , tType t2 == Executable = (ReplTargetGroup "" [t1,t2]:map singleGroup rs)
  | otherwise = map singleGroup tgts
  where
    singleGroup t = ReplTargetGroup (tName t) [t]

-- | Name of project
projectName :: FilePath -> String
projectName root = takeFileName $ dropTrailingPathSeparator root

-- | Get the cabal file in folder
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

-- | Read target groups from cabal file path
readTargetGroups :: FilePath -> IO [ReplTargetGroup]
readTargetGroups cabalFile = targetGroups <$> readTargets cabalFile

-- | Does a given path match the given target group (included in any source dir)
matchGroup :: FilePath -> ReplTargetGroup -> Bool
matchGroup fp gr = let
  fps = concatMap (normSourceDirs . tSourceDirs) $ rtgTargets gr
  in any (\r-> r=="" || r `isPrefixOf` fp) fps

-- | Normalize source dirs (handle . for example)
normSourceDirs :: [FilePath] -> [FilePath]
normSourceDirs [] = [""]
normSourceDirs rs = map norm rs
  where norm "." = ""
        norm "" = ""
        norm r
          | last r == '/' = r
          | otherwise = r ++ "/"

-- | Generate the module name from the file name, in the given target group
moduleName :: FilePath -> ReplTargetGroup -> Maybe String
moduleName fp gr = let
  fps = concatMap (normSourceDirs . tSourceDirs) $ rtgTargets gr
  in msum $ map match fps
  where
    match r
      | r=="" || r `isPrefixOf` fp = let
        rel = dropExtension $ drop (length r) fp
        in Just $ map rep rel
      | otherwise = Nothing
    rep c
     | c == '/' = '.'
     | c == '\\' = '.'
     | otherwise = c

    
  
