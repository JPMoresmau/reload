-- | Executable modul
module Main where

import           Language.Haskell.Reload (runApp)

import System.Environment

-- | Main entry point
main :: IO ()
main = do
  -- basic argument handling: the first arg is the port to listen to
  args <- getArgs
  let port = case args of
                (p:_) -> read p
                _ -> 8080
  runApp port
