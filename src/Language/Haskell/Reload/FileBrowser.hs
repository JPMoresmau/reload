{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
-- | Browse the folders and files inside our root folder
module Language.Haskell.Reload.FileBrowser
  ( FSItem(..)
  , listFiles
  , getMIMEText
  , HiddenFiles(..)
  )where

import Data.Typeable (Typeable)
import Data.Char (toLower)
import Data.List (sort,isPrefixOf)
import System.Directory
import System.FilePath
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
-- import qualified Data.ByteString as B
import qualified Data.Map as DM
import Network.Mime
import Data.Aeson
import Control.Monad
import Data.Default

-- | A File System item, wrapping the path
data FSItem =
    -- | A directory
    Dir
      {fsiPath :: FilePath}
  | -- | A path
    File
      {fsiPath :: FilePath
      ,fsiMime :: MimeType}
  deriving (Show,Read,Eq,Typeable)

-- | Instance for sorting
instance Ord FSItem where
  (Dir _)  <= (File _ _) = True
  (File _ _) <= (Dir _)  = False
  f1       <= f2       = can f1 <= can f2
    where can = map toLower . takeFileName . fsiPath

-- | Json Instance
instance ToJSON FSItem where
    toJSON (Dir p)= object ["type" .= ("dir"::T.Text),"path" .= p]
    toJSON (File p m)= object ["type" .= ("file"::T.Text),"path" .= p,"mime" .= T.decodeUtf8 m]
instance FromJSON FSItem where
    parseJSON (Object m) = do
        t <- m .: "type"
        case (t :: T.Text) of
          "dir" ->
              Dir <$> (m .: "path")
          "file" ->
                File <$> (m .: "path")
                     <*> (T.encodeUtf8 <$> m .: "mime")
          _   -> mzero
    parseJSON _          = mzero

data HiddenFiles = ShowHidden | HideHidden
  deriving (Show,Read,Eq,Ord,Bounded,Enum,Typeable)

instance Default HiddenFiles where
  def = HideHidden

-- | List all files inside a given directory
listFiles :: FilePath -> FilePath -> HiddenFiles -> IO [FSItem]
listFiles root cd hf = do
  fs <- getDirectoryContents $ root </> cd
  let visible = filter (isVisible . takeFileName) fs
  sort <$> mapM tofs visible
  where
    isVisible :: FilePath -> Bool
    isVisible fn 
      | fn == "." = False
      | fn == ".." = False
      | "." `isPrefixOf` fn = hf == ShowHidden
      | otherwise = True 
    tofs fp = do
      let full = root </> cd </> fp
      isFile <- doesFileExist full
      can <- canonicalizePath full
      let rel=makeRelative root can
      return $ if isFile
        then File rel $ getMIME rel
        else Dir rel



-- | Extended mime map with haskell and YAML
extendedMimeMap :: DM.Map Extension MimeType
extendedMimeMap = foldr (uncurry DM.insert) defaultMimeMap
                    [("hs","text/x-haskell"),("lhs","text/x-haskell"),
                     ("yaml","text/x-yaml"),("cabal","text/x-cabal")
                    ]



-- | Get the mime type for the given file name
getMIME :: FilePath -> MimeType
getMIME = mimeByExt extendedMimeMap "text/plain" . T.pack
--case takeExtension fp of
--  ".hs"  -> "haskell"
--  ".lhs" -> "haskell"
--  _      -> "text"

-- | Get MIME type as text
getMIMEText :: FilePath -> T.Text
getMIMEText = T.decodeUtf8 . getMIME

-- -- | Get the file contents as a Text. Assumes UTF-8 encoding
-- getFileContents :: FilePath -> IO T.Text
-- getFileContents fp = do
--   bs <- B.readFile fp
--   return $ T.decodeUtf8 bs

-- -- -- | Set the file contents as a Text. Assumes UTF-8 encoding
-- -- setFileContents :: FilePath -> T.Text -> IO ()
-- setFileContents fp cnts =
--   B.writeFile fp $ T.encodeUtf8 cnts
