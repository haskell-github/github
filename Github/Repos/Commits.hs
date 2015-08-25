-- | The repo commits API as described on
-- <http://developer.github.com/v3/repos/commits/>.
module Github.Repos.Commits (
 CommitQueryOption(..)
,commitsFor
,commitsFor'
,commitsWithOptionsFor'
,commit
,commit'
,commentsFor
,commentsFor'
,commitCommentsFor
,commitCommentsFor'
,commitCommentFor
,commitCommentFor'
,diff
,diff'
,module Github.Data
) where

import Github.Data
import Github.Private

import Data.Time.Format (iso8601DateFormat, formatTime)
import Data.Time (defaultTimeLocale)
import Data.List (intercalate)

githubFormat :: GithubDate -> String
githubFormat = formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S") . fromGithubDate

renderCommitQueryOption :: CommitQueryOption -> String
renderCommitQueryOption (CommitQuerySha sha) = "sha=" ++ sha
renderCommitQueryOption (CommitQueryPath path) = "path=" ++ path
renderCommitQueryOption (CommitQueryAuthor author) = "author=" ++ author
renderCommitQueryOption (CommitQuerySince date) = "since=" ++ ds ++ "Z"
    where ds = show $ githubFormat date
renderCommitQueryOption (CommitQueryUntil date) = "until=" ++ ds ++ "Z"
    where ds = show $ githubFormat date

-- | The commit history for a repo.
--
-- > commitsFor "mike-burns" "github"
commitsFor :: String -> String -> IO (Either Error [Commit])
commitsFor = commitsFor' Nothing

-- | The commit history for a repo.
-- With authentication.
--
-- > commitsFor' (Just (GithubBasicAuth (user, password))) "mike-burns" "github"
commitsFor' :: Maybe GithubAuth -> String -> String -> IO (Either Error [Commit])
commitsFor' auth user repo = githubGet' auth ["repos", user, repo, "commits"]

commitsWithOptionsFor :: String -> String -> [CommitQueryOption] -> IO (Either Error [Commit])
commitsWithOptionsFor = commitsWithOptionsFor' Nothing

-- | The commit history for a repo, with commits filtered to satisfy a list of
-- query options.
-- With authentication.
--
-- > commitsWithOptionsFor' (Just (GithubBasicAuth (user, password))) "mike-burns" "github" [CommitQueryAuthor "djeik"]
commitsWithOptionsFor' :: Maybe GithubAuth -> String -> String -> [CommitQueryOption] -> IO (Either Error [Commit])
commitsWithOptionsFor' auth user repo opts = githubGetWithQueryString' auth ["repos", user, repo, "commits"] qs
    where qs = intercalate "&" $ map renderCommitQueryOption opts

-- | Details on a specific SHA1 for a repo.
--
-- > commit "mike-burns" "github" "9d1a9a361266c3c890b1108ad2fdf52f824b1b81"
commit :: String -> String -> String -> IO (Either Error Commit)
commit = commit' Nothing

-- | Details on a specific SHA1 for a repo.
-- With authentication.
--
-- > commit (Just $ GithubBasicAuth (username, password)) "mike-burns" "github" "9d1a9a361266c3c890b1108ad2fdf52f824b1b81"
commit' :: Maybe GithubAuth -> String -> String -> String -> IO (Either Error Commit)
commit' auth user repo sha1 = githubGet' auth ["repos", user, repo, "commits", sha1]


-- | Details on a specific SHA1 for a repo.
-- With authentication.
--
-- > commit (Just $ GithubBasicAuth (username, password)) "mike-burns" "github" "9d1a9a361266c3c890b1108ad2fdf52f824b1b81"
commit' :: Maybe GithubAuth -> String -> String -> String -> IO (Either Error Commit)
commit' auth user repo sha1 = githubGet' auth ["repos", user, repo, "commits", sha1]


-- | All the comments on a Github repo.
--
-- > commentsFor "thoughtbot" "paperclip"
commentsFor :: String -> String -> IO (Either Error [Comment])
commentsFor = commentsFor' Nothing

-- | All the comments on a Github repo.
-- With authentication.
--
-- > commentsFor "thoughtbot" "paperclip"
commentsFor' :: Maybe GithubAuth -> String -> String -> IO (Either Error [Comment])
commentsFor' auth user repo = githubGet' auth ["repos", user, repo, "comments"]

-- | Just the comments on a specific SHA for a given Github repo.
--
-- > commitCommentsFor "thoughtbot" "paperclip" "41f685f6e01396936bb8cd98e7cca517e2c7d96b"
commitCommentsFor :: String -> String -> String -> IO (Either Error [Comment])
commitCommentsFor = commitCommentsFor' Nothing

-- | Just the comments on a specific SHA for a given Github repo.
-- With authentication.
--
-- > commitCommentsFor "thoughtbot" "paperclip" "41f685f6e01396936bb8cd98e7cca517e2c7d96b"
commitCommentsFor' :: Maybe GithubAuth -> String -> String -> String -> IO (Either Error [Comment])
commitCommentsFor' auth user repo sha1 =
  githubGet' auth ["repos", user, repo, "commits", sha1, "comments"]

-- | A comment, by its ID, relative to the Github repo.
--
-- > commitCommentFor "thoughtbot" "paperclip" "669575"
commitCommentFor :: String -> String -> String -> IO (Either Error Comment)
commitCommentFor = commitCommentFor' Nothing

-- | A comment, by its ID, relative to the Github repo.
--
-- > commitCommentFor "thoughtbot" "paperclip" "669575"
commitCommentFor' :: Maybe GithubAuth -> String -> String -> String -> IO (Either Error Comment)
commitCommentFor' auth user repo reqCommentId =
  githubGet' auth ["repos", user, repo, "comments", reqCommentId]

-- | The diff between two treeishes on a repo.
--
-- > diff "thoughtbot" "paperclip" "41f685f6e01396936bb8cd98e7cca517e2c7d96b" "HEAD"
diff :: String -> String -> String -> String -> IO (Either Error Diff)
diff = diff' Nothing

-- | The diff between two treeishes on a repo.
--
-- > diff "thoughtbot" "paperclip" "41f685f6e01396936bb8cd98e7cca517e2c7d96b" "HEAD"
diff' :: Maybe GithubAuth -> String -> String -> String -> String -> IO (Either Error Diff)
diff' auth user repo base headref =
  githubGet' auth ["repos", user, repo, "compare", base ++ "..." ++ headref]
