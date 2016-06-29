module Language.Haskell.Reload.ProjectSpec where

import Language.Haskell.Reload.Project

import           Test.Hspec

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