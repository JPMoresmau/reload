{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Language.Haskell.Reload.FileBrowserSpec (spec) where

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
spec = after cleanUp $ with app $ do
  describe "GET /files" $ do
    it "responds with files and folders at root" $ do
      js <- liftIO $ do
          root <- getCurrentDirectory
          fs <- getDirectoryContents root
          let visible = filter (not . ("." `isPrefixOf`) . takeFileName) fs
          sortBy sortFs <$> mapM (fileToJson root) visible
      get "/files" `shouldRespondWith` (fromValue $ toJSON js)
      get "/files/" `shouldRespondWith` (fromValue $ toJSON js)
    it "get source root folder" $ do
      get "/files/src" `shouldRespondWith` [json|[{"type":"dir","path":"src/Language"}]|]
      get "/files/src/" `shouldRespondWith` [json|[{"type":"dir","path":"src/Language"}]|]
    it "doesn't accept .." $ do
      get "/files/../reload/src" `shouldRespondWith` 404
    it "doesn't accept absolutes" $ do
      get "/files//tmp" `shouldRespondWith` 404
  describe "GET /file" $ do
    it "get root plain file" $ do
      testFile "LICENSE" "text/plain"
    it "get root cabal file" $ do
      testFile "reload.cabal" "text/x-cabal"
    it "get root yaml file" $ do
      testFile "stack.yaml" "text/x-yaml"
    it "get src haskell file" $ do
      testFile "src/Language/Haskell/Reload.hs" "text/x-haskell"
    it "doesn't accept .." $ do
        get "/file/../reload/LICENSE" `shouldRespondWith` 404
    it "doesn't accept absolutes" $ do
        get "/files//etc/passwd" `shouldRespondWith` 404
  describe "PUT /files" $ do
    it "creates a new folder" $ do
      liftIO $ do
          root <- getCurrentDirectory
          ex <- doesDirectoryExist (root </> "tempTest")
          when ex $ do
            removeDirectoryRecursive (root </> "tempTest")
            ex2 <- doesDirectoryExist (root </> "tempTest")
            ex2 `shouldBe` False
      put "/files/tempTest" B.empty `shouldRespondWith` [json|null|] {matchStatus = 201}
      liftIO $ do
          root <- getCurrentDirectory
          ex <- doesDirectoryExist (root </> "tempTest")
          ex `shouldBe` True
      put "/files/tempTest" B.empty `shouldRespondWith` [json|null|] {matchStatus = 204}
    it "doesn't accept .." $ do
        put "/files/../tempTest" B.empty `shouldRespondWith` 404
    it "doesn't accept absolutes" $ do
        put "/files//tmp/tempTest" B.empty `shouldRespondWith` 404
  describe "DELETE /files" $ do
    it "deletes an existing folder" $ do
      liftIO $ do
          root <- getCurrentDirectory
          createDirectoryIfMissing True (root </> "tempTest")
          ex <- doesDirectoryExist (root </> "tempTest")
          ex `shouldBe` True
      delete "/files/tempTest" `shouldRespondWith` [json|null|] {matchStatus = 200}
      liftIO $ do
          root <- getCurrentDirectory
          ex <- doesDirectoryExist (root </> "tempTest")
          ex `shouldBe` False
      delete "/files/tempTest" `shouldRespondWith` [json|null|] {matchStatus = 204}
    it "doesn't accept .." $ do
        delete "/files/../tempTest" `shouldRespondWith` 404
    it "doesn't accept absolutes" $ do
        delete "/files//tmp/tempTest" `shouldRespondWith` 404
  describe "PUT /file" $ do
    it "creates and modifies a new file" $ do
      liftIO $ do
          root <- getCurrentDirectory
          createDirectoryIfMissing True (root </> "tempTest")
          ex <- doesDirectoryExist (root </> "tempTest")
          ex `shouldBe` True
      put "/file/tempTest/f.txt" "content" `shouldRespondWith` 201
      liftIO $ do
          root <- getCurrentDirectory
          f <- B.readFile (root </> "tempTest" </> "f.txt")
          f `shouldBe` "content"
      put "/file/tempTest/f.txt" "dummy" `shouldRespondWith` 200
      liftIO $ do
          root <- getCurrentDirectory
          f <- B.readFile (root </> "tempTest" </> "f.txt")
          f `shouldBe` "dummy"
    it "doesn't accept .." $ do
        put "/file/../tempTest/f.txt" "dummy" `shouldRespondWith` 404
    it "doesn't accept absolutes" $ do
        put "/file//tmp/f.txt" "dummy" `shouldRespondWith` 404
    it "doesn't accept directories" $ do
        put "/file/test" "dummy" `shouldRespondWith` 403
  describe "DELETE /file" $ do
    it "deletes a file" $ do
      liftIO $ do
          root <- getCurrentDirectory
          createDirectoryIfMissing True (root </> "tempTest")
          ex <- doesDirectoryExist (root </> "tempTest")
          ex `shouldBe` True
          let p = root </> "tempTest" </> "f.txt"
          B.writeFile p "content"
          exf <- doesFileExist p
          exf `shouldBe` True
      delete "/file/tempTest/f.txt" `shouldRespondWith` [json|null|] {matchStatus = 200}
      liftIO $ do
        root <- getCurrentDirectory
        let p = root </> "tempTest" </> "f.txt"
        exf <- doesFileExist p
        exf `shouldBe` False
      delete "/file/tempTest/f.txt" `shouldRespondWith` [json|null|] {matchStatus = 204}
    it "doesn't accept .." $ do
      liftIO $ do
          root <- getCurrentDirectory
          createDirectoryIfMissing True (root </> "tempTest")
          ex <- doesDirectoryExist (root </> "tempTest")
          ex `shouldBe` True
          let p = root </> "tempTest" </> "f.txt"
          B.writeFile p "content"
          exf <- doesFileExist p
          exf `shouldBe` True
      delete "/file/../reload/tempTest/f.txt" `shouldRespondWith` 404
    it "doesn't accept absolutes" $ do
      delete "/file//tmp/f.txt" `shouldRespondWith` 404

cleanUp :: () -> IO()
cleanUp _ = do
    root <- getCurrentDirectory
    ex <- doesDirectoryExist (root </> "tempTest")
    when ex $ do
      removeDirectoryRecursive (root </> "tempTest")

--testFile :: FilePath -> ByteString -
testFile fp ct = do
  l <- liftIO $ B.readFile fp
  let fpb = B8.pack fp
  get ("/file/" <> fpb) `shouldRespondWith` 200 {matchHeaders = ["Content-Type" <:> ct]}
  r <- simpleBody <$> get ("/file/" <> fpb)
  liftIO $ r `shouldBe` l

fileToJson :: FilePath -> FilePath -> IO Value
fileToJson root fs = do
    let full =root</> fs
    isf <- doesFileExist full
    return $ if isf
        then let
          m =  HM.lookupDefault "text/plain" (takeExtension fs) myMimes
          in object ["type" .= ("file"::Text), "path" .= fs, "mime" .= m]
        else object ["type" .= ("dir"::Text), "path" .= fs]

sortFs :: Value -> Value -> Ordering
sortFs (Object m1) (Object m2) =
  let (String t1) = fromJust $ HM.lookup "type" m1
      (String t2) = fromJust $ HM.lookup "type" m2
      tc = t1 `compare` t2
  in if tc == EQ
      then
        let (String p1) = fromJust $ HM.lookup "path" m1
            (String p2) = fromJust $ HM.lookup "path" m2
        in (toLower p1) `compare` (toLower p2)
      else tc

myMimes :: HM.HashMap String Text
myMimes = HM.insert ".yaml" "text/x-yaml" $
          HM.insert ".cabal" "text/x-cabal" $
          HM.insert ".lhs" "text/x-haskell" $
          HM.insert ".hs" "text/x-haskell" HM.empty
