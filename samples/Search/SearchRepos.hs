
module SearchRepos where

import qualified Github.Search as Github
import Data.Maybe (fromMaybe)
import Data.List (intercalate)

import Data.Time.LocalTime (utc,utcToLocalTime,localDay)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Calendar (toGregorian)
import Text.Printf (printf)

today :: IO String
today = do
  now <- getCurrentTime
  let day = localDay $ utcToLocalTime utc now
      (y,m,d) = toGregorian day
   in return $ printf "%d-%02d-%02d" y m d

main = do
  day <- today
  putStrLn $ "today is " ++ day
  result <- Github.searchRepos $ "q=a in%3Aname language%3Ahaskell pushed%3A>" ++ day ++ "&per_page=100"
  putStrLn $ either (("Error: "++) . show) formatResult result 

formatResult r =(intercalate "\n\n" $ map formatRepo (Github.searchReposRepos r))
  ++ "\n\nCount: " ++ (show $ Github.searchReposTotalCount r)

formatRepo r =
  let fields = [ ("Name", Github.repoName)
                 ,("URL",  Github.repoHtmlUrl)
                 ,("Description", orEmpty . Github.repoDescription)
                 ,("Created-At", formatDate . Github.repoCreatedAt)
                 ,("Pushed-At", formatMaybeDate . Github.repoPushedAt)
               ]
  in intercalate "\n" $ map fmt fields
    where fmt (s,f) = fill 12 (s ++ ":") ++ " " ++ (f r)
          orEmpty = maybe "" id
          fill n s = s ++ (replicate n' ' ')
            where n' = max 0 (n - length s) 

formatMaybeDate = maybe "???" formatDate

formatDate = show . Github.fromGithubDate

