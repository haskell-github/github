module Github.Repos.Commits (
  commitsFor
, Commit(..)
, Author(..)
) where

import Data.Time

data Commit = Commit {
   commitSha     :: String
  ,commitAuthor  :: Author
  ,commitMessage :: String
}

data Author = Author {
   authorName  :: String
  ,authorEmail :: String
  ,authorDate  :: UTCTime
}

commitsFor :: String -> String -> IO (Either String [Commit])
commitsFor user repo = undefined
