{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Language.Haskell.Reload.Config where

import Language.Haskell.Reload.Build

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
        