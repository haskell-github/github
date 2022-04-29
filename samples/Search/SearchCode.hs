{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified GitHub
import Control.Monad (forM_)
import Data.List (intercalate)
import qualified Data.Text as T

main :: IO ()
main = do
  let query = "Code repo:haskell-github/github"
  result <- GitHub.github' GitHub.searchCodeR query 1000
  case result of
    Left e  -> putStrLn $ "Error: " ++ show e
    Right r -> do
      forM_ (GitHub.searchResultResults r) $ \r -> do
        putStrLn $ formatCode r
        putStrLn ""
      putStrLn $ "Count: " ++ show (GitHub.searchResultTotalCount r)
        ++ " matches for the query: \"" ++ T.unpack query ++ "\""

formatCode :: GitHub.Code -> String
formatCode r =
  let fields = [ ("Name", show . GitHub.codeName)
               , ("Path", show . GitHub.codePath)
               , ("Sha",  show . GitHub.codeSha)
               , ("URL",  show . GitHub.codeHtmlUrl)
               ]
  in intercalate "\n" $ map fmt fields
    where fmt (s,f) = fill 12 (s ++ ":") ++ " " ++ f r
          fill n s = s ++ replicate n' ' '
            where n' = max 0 (n - length s) 
