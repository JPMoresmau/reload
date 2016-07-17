{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Language.Haskell.Reload.Config where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as BS
import System.Directory
import qualified Data.Text as T
import Data.Maybe

config :: IO (Maybe Value)
config = do
  let configFile = "reload.json"
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

formatCommand :: Maybe Value -> String
formatCommand mv =
  case mv of
    Nothing -> defCommand
    Just v -> fromMaybe defCommand $ parseMaybe (\_->withObject "Config" extractEditor v) T.null
  where
    defCommand = "stylish-haskell"
    extractEditor o = do
      me <- o .:? "editor"
      case me of
        Nothing -> return defCommand
        Just e -> withObject "Editor" extractActions e
    extractActions o = do
      ma <- o .:? "actions"
      case ma of
        Nothing -> return defCommand
        Just a -> withObject "Actions" extractFormat a
    extractFormat o = do
      mf <- o .:? "format"
      case mf of
        Just (String f) -> return $ T.unpack f
        _ -> return defCommand
        

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