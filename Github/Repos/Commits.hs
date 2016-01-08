{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The repo commits API as described on
-- <http://developer.github.com/v3/repos/commits/>.
module Github.Repos.Commits (
    CommitQueryOption(..),
    commitsFor,
    commitsFor',
    commitsForR,
    commitsWithOptionsFor,
    commitsWithOptionsFor',
    commitsWithOptionsForR,
    commit,
    commit',
    commitR,
    diff,
    diff',
    diffR,
    module Github.Data,
    ) where

import Data.Monoid    ((<>))
import Data.Vector    (Vector)
import Github.Auth
import Github.Data
import Github.Request

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text.Encoding    as TE

import Data.Time.Format (formatTime)
#if MIN_VERSION_time (1,5,0)
import Data.Time        (defaultTimeLocale)
import Data.Time.Format (iso8601DateFormat)
#else
import System.Locale (defaultTimeLocale)
#endif

githubFormat :: GithubDate -> String
#if MIN_VERSION_time (1,5,0)
githubFormat = formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S") . fromGithubDate
#else
githubFormat = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" . fromGithubDate
#endif

renderCommitQueryOption :: CommitQueryOption -> (BS.ByteString, Maybe BS.ByteString)
renderCommitQueryOption (CommitQuerySha sha)      = ("sha", Just $ TE.encodeUtf8 sha)
renderCommitQueryOption (CommitQueryPath path)     = ("path", Just $ TE.encodeUtf8 path)
renderCommitQueryOption (CommitQueryAuthor author) = ("author", Just $ TE.encodeUtf8 author)
renderCommitQueryOption (CommitQuerySince date)    = ("since", Just $ BS8.pack ds <> "Z")
    where ds = show $ githubFormat date
renderCommitQueryOption (CommitQueryUntil date)    = ("until", Just $ BS8.pack ds <> "Z")
    where ds = show $ githubFormat date

-- | The commit history for a repo.
--
-- > commitsFor "mike-burns" "github"
commitsFor :: Name GithubOwner -> Name Repo -> IO (Either Error (Vector Commit))
commitsFor = commitsFor' Nothing

-- | The commit history for a repo.
-- With authentication.
--
-- > commitsFor' (Just (GithubBasicAuth (user, password))) "mike-burns" "github"
commitsFor' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> IO (Either Error (Vector Commit))
commitsFor' auth user repo =
    commitsWithOptionsFor' auth user repo []

-- | List commits on a repository.
-- See <https://developer.github.com/v3/repos/commits/#list-commits-on-a-repository>
commitsForR :: Name GithubOwner -> Name Repo -> GithubRequest k (Vector Commit)
commitsForR user repo = commitsWithOptionsForR user repo []

commitsWithOptionsFor :: Name GithubOwner -> Name Repo -> [CommitQueryOption] -> IO (Either Error (Vector Commit))
commitsWithOptionsFor = commitsWithOptionsFor' Nothing

-- | The commit history for a repo, with commits filtered to satisfy a list of
-- query options.
-- With authentication.
--
-- > commitsWithOptionsFor' (Just (GithubBasicAuth (user, password))) "mike-burns" "github" [CommitQueryAuthor "djeik"]
commitsWithOptionsFor' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> [CommitQueryOption] -> IO (Either Error (Vector Commit))
commitsWithOptionsFor' auth user repo opts =
    executeRequestMaybe auth $ commitsWithOptionsForR user repo opts

-- | List commits on a repository.
-- See <https://developer.github.com/v3/repos/commits/#list-commits-on-a-repository>
commitsWithOptionsForR :: Name GithubOwner -> Name Repo -> [CommitQueryOption] -> GithubRequest k (Vector Commit)
commitsWithOptionsForR user repo opts =
    GithubPagedGet ["repos", untagName user, untagName repo, "commits"] qs
  where
    qs = map renderCommitQueryOption opts


-- | Details on a specific SHA1 for a repo.
--
-- > commit "mike-burns" "github" "9d1a9a361266c3c890b1108ad2fdf52f824b1b81"
commit :: Name GithubOwner -> Name Repo -> Name Commit -> IO (Either Error Commit)
commit = commit' Nothing

-- | Details on a specific SHA1 for a repo.
-- With authentication.
--
-- > commit (Just $ GithubBasicAuth (username, password)) "mike-burns" "github" "9d1a9a361266c3c890b1108ad2fdf52f824b1b81"
commit' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> Name Commit -> IO (Either Error Commit)
commit' auth user repo sha =
    executeRequestMaybe auth $ commitR user repo sha

-- | Get a single commit
-- See <https://developer.github.com/v3/repos/commits/#get-a-single-commit>
commitR :: Name GithubOwner -> Name Repo -> Name Commit -> GithubRequest k Commit
commitR user repo sha =
    GithubGet ["repos", untagName user, untagName repo, "commits", untagName sha] []

-- | The diff between two treeishes on a repo.
--
-- > diff "thoughtbot" "paperclip" "41f685f6e01396936bb8cd98e7cca517e2c7d96b" "HEAD"
diff :: Name GithubOwner -> Name Repo -> Name Commit -> Name Commit -> IO (Either Error Diff)
diff = diff' Nothing

-- | The diff between two treeishes on a repo.
--
-- > diff "thoughtbot" "paperclip" "41f685f6e01396936bb8cd98e7cca517e2c7d96b" "HEAD"
diff' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> Name Commit -> Name Commit -> IO (Either Error Diff)
diff' auth user repo base headref =
    executeRequestMaybe auth $ diffR user repo base headref

-- | Compare two commits
-- See <https://developer.github.com/v3/repos/commits/#compare-two-commits>
diffR :: Name GithubOwner -> Name Repo -> Name Commit -> Name Commit -> GithubRequest k Diff
diffR user repo base headref =
    GithubGet ["repos", untagName user, untagName repo, "compare", untagName base ++ "..." ++ untagName headref] []
