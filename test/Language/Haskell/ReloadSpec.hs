{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Language.Haskell.ReloadSpec (spec) where

import           Test.Hspec
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON
import           Data.Aeson (Value(..), object, (.=))

import           Language.Haskell.Reload (app)

spec :: Spec
spec = with app $ do
  describe "GET /" $ do
    it "responds with 200 /" $ do
      get "/" `shouldRespondWith` 200
    it "doesn't accept .. " $ do
      get "/images/../../app/Main.hs" `shouldRespondWith` 404
    it "doesn't accept absolute " $ do
      get "//etc/passwd" `shouldRespondWith` 404

--    it "has 'Content-Type: text/plain; charset=utf-8'" $ do
--      get "/" `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> "text/plain; charset=utf-8"]}
