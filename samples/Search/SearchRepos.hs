{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified GitHub
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Data.List (intercalate)
import System.Environment (getArgs)
import Text.Printf (printf)
import Data.Time.Clock (getCurrentTime, UTCTime(..))
import Data.Time.LocalTime (utc,utcToLocalTime,localDay)
import Data.Time.Calendar (toGregorian)
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = do
  args <- getArgs
  date <- case args of
            (x:_)     -> return $ T.pack x
            _ -> today
  let query = ("language:haskell created:>" <> date) :: Text
  result <- GitHub.github' GitHub.searchReposR query 1000
  case result of
    Left e  -> putStrLn $ "Error: " ++ show e
    Right r -> do
      forM_ (GitHub.searchResultResults r) $ \r -> do
        putStrLn $ formatRepo r
        putStrLn ""
      putStrLn $ "Count: " ++ show (GitHub.searchResultTotalCount r)
        ++ " Haskell repos created since " ++ T.unpack date

-- | return today (in UTC) formatted as YYYY-MM-DD
today :: IO Text
today = do
  now <- getCurrentTime
  let day = localDay $ utcToLocalTime utc now
      (y,m,d) = toGregorian day
   in return $ T.pack $ printf "%d-%02d-%02d" y m d

formatRepo :: GitHub.Repo -> String
formatRepo r =
  let fields = [ ("Name", show . GitHub.repoName)
                 ,("URL",  show . GitHub.repoHtmlUrl)
                 ,("Description", show . orEmpty . GitHub.repoDescription)
                 ,("Created-At", formatMaybeDate . GitHub.repoCreatedAt)
                 ,("Pushed-At", formatMaybeDate . GitHub.repoPushedAt)
                 ,("Stars", show . GitHub.repoStargazersCount)
               ]
  in intercalate "\n" $ map fmt fields
    where fmt (s,f) = fill 12 (s ++ ":") ++ " " ++ f r
          orEmpty = fromMaybe ""
          fill n s = s ++ replicate n' ' '
            where n' = max 0 (n - length s) 


formatMaybeDate :: Maybe UTCTime -> String
formatMaybeDate = maybe "???" show
