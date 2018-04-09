{-# LANGUAGE OverloadedStrings #-}
module AllHaskellRepos where
import           Control.Monad(when)
import           Data.List(group, sort)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import           Data.Time.Calendar(addDays, Day(..), showGregorian)
import           Data.Time.Clock(getCurrentTime, UTCTime(..))
import           Data.Time.Format(parseTimeM, defaultTimeLocale, iso8601DateFormat)
import           Time.System(dateCurrent)
import           GitHub.Auth(Auth(..))
import           GitHub.Endpoints.Search(searchRepos', SearchResult(..), EscapeItem(..),
                                         searchIssues')
import           GitHub.Data.Repos
import           GitHub.Data.Definitions
import           GitHub.Data.Name
import           GitHub.Data.URL
import           GitHub.Data.Options(SearchRepoMod(..), SearchRepoOptions(..), Language(..),
                                     License(..), StarsForksUpdated(..), SortDirection(..),
                                     searchRepoModToQueryString)
import           System.FilePath.Posix(FilePath)
import Debug.Trace

-- | A search query finds all Haskell libraries on github
--   and updates two files of all packages/authors
updateGithub :: [FilePath] -> IO ()
updateGithub [lastIntervalEnd, authorsCsv, packagesCsv] = do
  lastEnd <- T.readFile lastIntervalEnd -- first time: 2008-03-01
  start <- parseTimeM True defaultTimeLocale (iso8601DateFormat Nothing) (T.unpack lastEnd)
  intervals pass start 10 -- stop after 10 queries
  a <- T.readFile authorsCsv
  T.writeFile authorsCsv  (dups a)
  p <- T.readFile packagesCsv
  T.writeFile packagesCsv (dups p)
 where
  dups = T.unlines . map head . group . sort . T.lines
  -- Go through all github repos, by chosing small time intervals
  intervals :: String -> Day -> Int -> IO ()
  intervals pass start i = do
    let newDate = addDays 10 start -- assuming less than 100 repos in 10 days

    -- Remember the last succesfully scanned interval 
    -- (to update the list and continue when query timeout reached or query failed)
    T.writeFile lastIntervalEnd (T.pack (showGregorian newDate))

-- https://api.github.com/search/repositories?q=language:haskell+created:2009-01-01..2009-02-01&sort=stars&order=desc
    let query search = search { searchRepoOptionsLanguage = Just (Language "Haskell")
                              , searchRepoOptionsSortBy   = Just Stars
                              , searchRepoOptionsOrder    = Just SortDescending
                              , searchRepoOptionsCreated  = Just (start, newDate)
                              }
    res <- searchRepos' (Just $ BasicAuth "user" "pass") (SearchRepoMod query)
    either (\_-> return ()) appendToCSV res
--    putStrLn (show res) -- for debugging
    currentDate <- fmap utctDay getCurrentTime
    when (newDate < currentDate && i>0) (intervals pass newDate (i-1))

  appendToCSV :: SearchResult Repo -> IO ()
  appendToCSV res = do
    V.mapM_ extractFromRepo (searchResultResults res)
   where
    extractFromRepo r = do
      T.appendFile authorsCsv  (untagName (simpleOwnerLogin (repoOwner r)) `T.append` "\n")
      T.appendFile packagesCsv (getUrl (repoHtmlUrl r) `T.append` "\n")

