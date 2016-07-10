{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Language.Haskell.Reload.ProjectSpec where

import           Language.Haskell.Reload (app)
import Language.Haskell.Reload.Project

import           Test.Hspec
import System.Directory
import System.FilePath
import           Test.Hspec.Wai
import           Test.Hspec.Wai.JSON

spec :: Spec
spec = do
  describe "target groups" $ do
    it "combines Library and First Executable" $ do
      let tgts=[Target Library "" ["src"],Target Executable "exec" ["exe"]]
      targetGroups tgts `shouldBe` [ReplTargetGroup "" [Target Library "" ["src"],Target Executable "exec" ["exe"]]]
    it "doesn't combine Library and Test" $ do
      let tgts=[Target Library "" ["src"],Target TestSuite "test" ["test"]]
      targetGroups tgts `shouldBe` [ReplTargetGroup "" [Target Library "" ["src"]],ReplTargetGroup "test" [Target TestSuite "test" ["test"]]]
  describe "target group paths" $ do
    it "resolves explicit paths" $ do
      let tg1=ReplTargetGroup "" [Target Library "" ["src"],Target Executable "exec" ["exe"]]
      matchGroup "src/Reload.hs" tg1 `shouldBe` True
      matchGroup "exe/Main.hs" tg1 `shouldBe` True
      matchGroup "test/Reload.hs" tg1 `shouldBe` False
    it "resolve . paths" $ do
      let tg1=ReplTargetGroup "" [Target Library "" ["."]]
      matchGroup "Reload.hs" tg1 `shouldBe` True
      matchGroup "src/Reload.hs" tg1 `shouldBe` True
    it "resolve missing paths" $ do
      let tg1=ReplTargetGroup "" [Target Library "" []]
      matchGroup "Reload.hs" tg1 `shouldBe` True
      matchGroup "src/Reload.hs" tg1 `shouldBe` True
    it "resolve prefixes properly" $ do
      let tg1=ReplTargetGroup "" [Target Library "" ["src"],Target Executable "exec" ["exe"]]
      matchGroup "src2/Reload.hs" tg1 `shouldBe` False
  describe "find cabal file" $ do
    it "find our own file" $ do
      dir <- getCurrentDirectory
      cf <- cabalFileInFolder dir
      cf `shouldBe` Just (dir </> "reload.cabal")
    it "find our own file with trailing slash" $ do
        dir <- getCurrentDirectory
        cf <- cabalFileInFolder $ dir ++ "/"
        cf `shouldBe` Just (dir </> "reload.cabal")
    it "doesn't find a non existing file" $ do
      dir <- getCurrentDirectory
      cf <- cabalFileInFolder $ dir </> "src"
      cf `shouldBe` Nothing
  describe "load targets and groups" $ do
    it "load our own file" $ do
      dir <- getCurrentDirectory
      Just cf <- cabalFileInFolder dir
      tgts <- readTargets cf
      tgts `shouldBe` [Target Library "" ["src"],Target Executable "reload-exe" ["app"],Target TestSuite "reload-test" ["src","test"]]
      let grps = targetGroups tgts
      grps `shouldBe` [ReplTargetGroup "" [Target Library "" ["src"],Target Executable "reload-exe" ["app"]],ReplTargetGroup "reload-test" [Target TestSuite "reload-test" ["src","test"]]]
  describe "GET targets"$ do
    with (app False) $ 
      it "get our own targets" $ do
        get "/targets" `shouldRespondWith` [json|[{name:"",type:"Library"},{name:"reload-exe",type:"Executable"},{name:"reload-test",type:"TestSuite"}]|] {matchStatus = 200}
  describe "GET targetGroupss"$ do
    with (app False) $ 
      it "get our own target groups" $ do
        get "/targetGroups" `shouldRespondWith` [json|[{name:"",targets:[{name:"",type:"Library"},{name:"reload-exe",type:"Executable"}]},{name:"reload-test",targets:[{name:"reload-test",type:"TestSuite"}]}]|] {matchStatus = 200}
  