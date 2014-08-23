{-# LANGUAGE OverloadedStrings #-}
module SearchCode where

import qualified Github.Search as Github
import qualified Github.Data as Github
import Control.Monad (forM,forM_)
import Data.Maybe (fromMaybe)
import Data.List (intercalate)

main = do
  let query = "q=Code repo:jwiegley/github&per_page=100"
  let auth = Nothing
  result <- Github.searchCode' auth query
  case result of
    Left e  -> putStrLn $ "Error: " ++ show e
    Right r -> do forM_ (Github.searchCodeCodes r) (\r -> do
                    putStrLn $ formatCode r
                    putStrLn ""
                    )
                  putStrLn $ "Count: " ++ show n ++ " matches for the query: \"" ++ query ++ "\""
      where n = Github.searchCodeTotalCount r

formatCode :: Github.Code -> String
formatCode r =
  let fields = [ ("Name", Github.codeName)
                 ,("Path",  Github.codePath)
                 ,("Sha", Github.codeSha)
                 ,("URL", Github.codeHtmlUrl)
               ]
  in intercalate "\n" $ map fmt fields
    where fmt (s,f) = fill 12 (s ++ ":") ++ " " ++ f r
          fill n s = s ++ replicate n' ' '
            where n' = max 0 (n - length s) 

