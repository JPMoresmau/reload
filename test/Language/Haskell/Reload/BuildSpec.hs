{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Language.Haskell.Reload.BuildSpec (spec) where


import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Network.Wai.Test
import           Data.Aeson hiding (json)

import           Language.Haskell.Reload (app)
import           System.Directory
import           System.FilePath
import           Data.List (sortBy, isPrefixOf)
import           Data.Text (Text,toLower)
import qualified Data.HashMap.Strict as HM
import           Data.Ord
import           Data.Maybe
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as B8
import           Data.Monoid
import           Control.Monad

spec :: Spec
spec = with (app True)$ do
  describe "info" $ do
    it "get known info in module" $ do
      get "/info/src/Language/Haskell/Reload/Build.hs?word=tryPutMVar" `shouldRespondWith` [json|["tryPutMVar :: MVar a -> a -> IO Bool \t-- Defined in ‘GHC.MVar’"]|] {matchStatus = 200}
  describe "complete" $ do
    it "provide import complete" $ do
      get "/complete/src/Language/Haskell/Reload/Build.hs?word=import Data.May" `shouldRespondWith` [json|["Data.Maybe"]|] {matchStatus = 200}
      get "/complete/src/Language/Haskell/Reload/Build.hs?word=import Data.Map" `shouldRespondWith` [json|["Data.Map","Data.Map.Lazy","Data.Map.Strict"]|] {matchStatus = 200}
    it "provide code complete" $ do
      get "/complete/src/Language/Haskell/Reload/Build.hs?word=inf" `shouldRespondWith` [json|["info"]|] {matchStatus = 200}
      get "/complete/src/Language/Haskell/Reload/Build.hs?word=comp" `shouldRespondWith` [json|["compare","complete"]|] {matchStatus = 200}