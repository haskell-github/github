{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified GitHub.Data.Search as Github (searchResultTotalCount, searchResultResults)
import qualified GitHub.Data.Repos as Github (Repo(..))
import qualified GitHub.Endpoints.Search as Github (searchRepos')
import Control.Monad (forM_)
import System.Environment (getArgs)
import Text.Printf (printf)
import Data.Time.Clock (getCurrentTime)
import Data.Time.LocalTime (utc,utcToLocalTime,localDay)
import Data.Time.Calendar (toGregorian)

import GitHub.Internal.Prelude (pack)


main :: IO ()
main = do
  args <- getArgs
  date <- case args of
            (x:_)     -> return x
            otherwise -> today
  let qs = [("q", "language:haskell created:<" `mappend` pack date), ("per_page", "100"), ("page", "2")]
  let auth = Nothing
  result <- Github.searchRepos' auth qs
  case result of
    Left e  -> putStrLn $ "Error: " ++ show e
    Right r -> do forM_ (Github.searchResultResults r) (\r -> do
                    putStrLn $ formatRepo r
                    putStrLn ""
                    )
                  putStrLn $ "Count: " ++ show n ++ " Haskell repos created since " ++ date
                  putStrLn $ show $ length $ Github.searchResultResults r
      where n = Github.searchResultTotalCount r

-- | return today (in UTC) formatted as YYYY-MM-DD
today :: IO String
today = do
  now <- getCurrentTime
  let day = localDay $ utcToLocalTime utc now
      (y,m,d) = toGregorian day
   in return $ printf "%d-%02d-%02d" y m d

formatRepo :: Github.Repo -> String
formatRepo = show
