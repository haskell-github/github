{-# LANGUAGE OverloadedStrings #-}
module SearchRepos where

import qualified Github.Search as Github
import qualified Github.Data as Github
import Control.Monad (forM,forM_)
import Data.Maybe (fromMaybe)
import Data.List (intercalate)
import System.Environment (getArgs)
import Text.Printf (printf)
import Data.Time.Clock (getCurrentTime, UTCTime(..))
import Data.Time.LocalTime (utc,utcToLocalTime,localDay,localTimeOfDay,TimeOfDay(..))
import Data.Time.Calendar (toGregorian)

main = do
  args <- getArgs
  date <- case args of
            (x:_)     -> return x
            otherwise -> today
  let query = "q=language%3Ahaskell created%3A>" ++ date ++ "&per_page=100"
  let auth = Nothing
  result <- Github.searchRepos' auth query
  case result of
    Left e  -> putStrLn $ "Error: " ++ show e
    Right r -> do forM_ (Github.searchReposRepos r) (\r -> do
                    putStrLn $ formatRepo r
                    putStrLn ""
                    )
                  putStrLn $ "Count: " ++ show n ++ " Haskell repos created since " ++ date
      where n = Github.searchReposTotalCount r

-- | return today (in UTC) formatted as YYYY-MM-DD
today :: IO String
today = do
  now <- getCurrentTime
  let day = localDay $ utcToLocalTime utc now
      (y,m,d) = toGregorian day
   in return $ printf "%d-%02d-%02d" y m d

formatRepo :: Github.Repo -> String
formatRepo r =
  let fields = [ ("Name", Github.repoName)
                 ,("URL",  Github.repoHtmlUrl)
                 ,("Description", orEmpty . Github.repoDescription)
                 ,("Created-At", formatMaybeDate . Github.repoCreatedAt)
                 ,("Pushed-At", formatMaybeDate . Github.repoPushedAt)
                 ,("Stars", show . Github.repoStargazersCount)
               ]
  in intercalate "\n" $ map fmt fields
    where fmt (s,f) = fill 12 (s ++ ":") ++ " " ++ f r
          orEmpty = fromMaybe ""
          fill n s = s ++ replicate n' ' '
            where n' = max 0 (n - length s) 

formatMaybeDate = maybe "???" formatDate
formatDate = show . Github.fromGithubDate
