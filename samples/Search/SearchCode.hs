{-# LANGUAGE OverloadedStrings #-}

module SearchCode where

import qualified Github as Github
import Control.Monad (forM_)
import Data.List (intercalate)

main :: IO ()
main = do
  let query = "q=Code repo:jwiegley/github&per_page=100"
  result <- Github.github' Github.searchCodeR query 1000
  case result of
    Left e  -> putStrLn $ "Error: " ++ show e
    Right r -> do
      forM_ (Github.searchResultResults r) $ \r -> do
        putStrLn $ formatCode r
        putStrLn ""
      putStrLn $ "Count: " ++ show (Github.searchResultTotalCount r)
        ++ " matches for the query: \"" ++ T.unpack query ++ "\""

formatCode :: Github.Code -> String
formatCode r =
  let fields = [ ("Name", show . Github.codeName)
               , ("Path", show . Github.codePath)
               , ("Sha",  show . Github.codeSha)
               , ("URL",  show . Github.codeHtmlUrl)
               ]
  in intercalate "\n" $ map fmt fields
    where fmt (s,f) = fill 12 (s ++ ":") ++ " " ++ f r
          fill n s = s ++ replicate n' ' '
            where n' = max 0 (n - length s) 
