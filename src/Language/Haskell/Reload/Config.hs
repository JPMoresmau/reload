{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
-- | Configuration handling
module Language.Haskell.Reload.Config where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BS
import System.Directory
import qualified Data.Text as T
import Data.Maybe
import System.FilePath

-- | Get the configuration JSON if it exists in the given folder
config :: FilePath -> IO (Maybe Value)
config root = do
  let configFile = root </> "reload.json"
  ex <- doesFileExist configFile
  if ex 
    then do
      bs <- BS.readFile configFile
      let ev = eitherDecode bs
      case ev of
        Left s -> do
          putStrLn "Cannot parse config file reload.json"
          putStrLn s
          return Nothing
        Right v -> return v
    else return Nothing

-- | Get the format command (default is stylish-haskell)
formatCommand :: Maybe Value -> String
formatCommand mv = extract mv defCommand ["editor","actions","format"] 
  (\mf->  case mf of
            Just (String f) -> T.unpack f
            _ -> defCommand)
  where
     defCommand = "stylish-haskell"
  
-- | Should we show hidden files?
showHiddenFiles :: Maybe Value -> Bool
showHiddenFiles mv = extract mv defShow ["files","hidden"] 
  (\mf -> case mf of
            Just (Bool b) -> b
            _ -> defShow)
  where
    defShow = False

-- | Extract a value from JSON, following down a path
extract :: Maybe Value -> a -> [T.Text] -> (Maybe Value -> a) -> a
extract mv defV paths extr = 
  case mv of
    Nothing -> defV
    Just v -> fromMaybe defV $ parseMaybe (\_->withObject "Config" (extracts paths) v) T.null
  where
    extracts [t] o = do
      mf <- o .:? t
      return $ extr mf
    extracts [] _ = return defV
    extracts (t:ts) o = do
      me <- o .:? t
      case me of
        Nothing -> return defV
        Just e -> withObject (T.unpack t) (extracts ts) e    
  
{--
import Data.Aeson
import Data.Default
import qualified Data.Text as T

data ReloadState = ReloadState
  { cBuildState :: BuildState
  , cConfiguration :: Configuration
  }

data Configuration = Configuration
  { cCommands::Commands
  } deriving (Read,Show,Eq,Ord)
  
instance Default Configuration where
  def = Configuration def

instance ToJSON Configuration where
  toJSON (Configuration{..}) = object ["commands" .= cCommands]
  
instance FromJSON Configuration where
  parseJSON (Object o) = Configuration 
                          <$> o .:? "commands" .!= def
  parseJSON _ = fail "Configuration"

data Commands = Commands
  { buildCmd :: Maybe T.Text
  , testCmd :: Maybe T.Text
  , benchCmd :: Maybe T.Text
  , docCmd :: Maybe T.Text
  } deriving (Read,Show,Eq,Ord)

instance Default Commands where
  def  = Commands Nothing Nothing Nothing Nothing

instance ToJSON Commands where
  toJSON (Commands{..}) = object ["build" .= buildCmd,"test" .= testCmd,"bench" .= benchCmd,"doc" .= docCmd]
  
instance FromJSON Commands where
  parseJSON (Object o) = Commands 
                          <$> o .:? "build"
                          <*> o .:? "test"
                          <*> o .:? "bench"
                          <*> o .:? "doc"
  parseJSON _ = fail "Commands"
  
buildCommand :: ReloadState -> IO String
buildCommand = getCommand buildCmd ("cabal build","stack build")

testCommand :: ReloadState -> IO String
testCommand = getCommand testCmd ("cabal test","stack test")

benchCommand :: ReloadState -> IO String
benchCommand = getCommand benchCmd ("cabal bench","stack bench")
  
getCommand ::(Commands -> Maybe T.Text) -> (T.Text,T.Text) ->  ReloadState -> IO String
getCommand getter (defCabal,defStack) rs = do
  t <- case getter $ cCommands $ cConfiguration rs of
        Just t -> return t
        Nothing -> do
          st <- hasStack (bsRoot $ cBuildState rs)
          return $ if st then defStack else defCabal
  return $ T.unpack t
--}