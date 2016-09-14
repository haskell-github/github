{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The repo commits API as described on
-- <http://developer.github.com/v3/repos/commits/>.
module GitHub.Endpoints.Repos.Commits (
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
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

import qualified Data.ByteString    as BS
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE

renderCommitQueryOption :: CommitQueryOption -> (BS.ByteString, Maybe BS.ByteString)
renderCommitQueryOption (CommitQuerySha sha)      = ("sha", Just $ TE.encodeUtf8 sha)
renderCommitQueryOption (CommitQueryPath path)     = ("path", Just $ TE.encodeUtf8 path)
renderCommitQueryOption (CommitQueryAuthor author) = ("author", Just $ TE.encodeUtf8 author)
renderCommitQueryOption (CommitQuerySince date)    = ("since", Just $ TE.encodeUtf8 . T.pack $ formatISO8601 date)
renderCommitQueryOption (CommitQueryUntil date)    = ("until", Just $ TE.encodeUtf8 . T.pack $ formatISO8601 date)

-- | The commit history for a repo.
--
-- > commitsFor "mike-burns" "github"
commitsFor :: Name Owner -> Name Repo -> IO (Either Error (Vector Commit))
commitsFor = commitsFor' Nothing

-- | The commit history for a repo.
-- With authentication.
--
-- > commitsFor' (Just (BasicAuth (user, password))) "mike-burns" "github"
commitsFor' :: Maybe Auth -> Name Owner -> Name Repo -> IO (Either Error (Vector Commit))
commitsFor' auth user repo =
    commitsWithOptionsFor' auth user repo []

-- | List commits on a repository.
-- See <https://developer.github.com/v3/repos/commits/#list-commits-on-a-repository>
commitsForR :: Name Owner -> Name Repo -> FetchCount -> Request k (Vector Commit)
commitsForR user repo limit = commitsWithOptionsForR user repo limit []

commitsWithOptionsFor :: Name Owner -> Name Repo -> [CommitQueryOption] -> IO (Either Error (Vector Commit))
commitsWithOptionsFor = commitsWithOptionsFor' Nothing

-- | The commit history for a repo, with commits filtered to satisfy a list of
-- query options.
-- With authentication.
--
-- > commitsWithOptionsFor' (Just (BasicAuth (user, password))) "mike-burns" "github" [CommitQueryAuthor "djeik"]
commitsWithOptionsFor' :: Maybe Auth -> Name Owner -> Name Repo -> [CommitQueryOption] -> IO (Either Error (Vector Commit))
commitsWithOptionsFor' auth user repo opts =
    executeRequestMaybe auth $ commitsWithOptionsForR user repo FetchAll opts

-- | List commits on a repository.
-- See <https://developer.github.com/v3/repos/commits/#list-commits-on-a-repository>
commitsWithOptionsForR :: Name Owner -> Name Repo -> FetchCount -> [CommitQueryOption] -> Request k (Vector Commit)
commitsWithOptionsForR user repo limit opts =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "commits"] qs limit
  where
    qs = map renderCommitQueryOption opts


-- | Details on a specific SHA1 for a repo.
--
-- > commit "mike-burns" "github" "9d1a9a361266c3c890b1108ad2fdf52f824b1b81"
commit :: Name Owner -> Name Repo -> Name Commit -> IO (Either Error Commit)
commit = commit' Nothing

-- | Details on a specific SHA1 for a repo.
-- With authentication.
--
-- > commit (Just $ BasicAuth (username, password)) "mike-burns" "github" "9d1a9a361266c3c890b1108ad2fdf52f824b1b81"
commit' :: Maybe Auth -> Name Owner -> Name Repo -> Name Commit -> IO (Either Error Commit)
commit' auth user repo sha =
    executeRequestMaybe auth $ commitR user repo sha

-- | Query a single commit.
-- See <https://developer.github.com/v3/repos/commits/#get-a-single-commit>
commitR :: Name Owner -> Name Repo -> Name Commit -> Request k Commit
commitR user repo sha =
    query ["repos", toPathPart user, toPathPart repo, "commits", toPathPart sha] []

-- | The diff between two treeishes on a repo.
--
-- > diff "thoughtbot" "paperclip" "41f685f6e01396936bb8cd98e7cca517e2c7d96b" "HEAD"
diff :: Name Owner -> Name Repo -> Name Commit -> Name Commit -> IO (Either Error Diff)
diff = diff' Nothing

-- | The diff between two treeishes on a repo.
--
-- > diff "thoughtbot" "paperclip" "41f685f6e01396936bb8cd98e7cca517e2c7d96b" "HEAD"
diff' :: Maybe Auth -> Name Owner -> Name Repo -> Name Commit -> Name Commit -> IO (Either Error Diff)
diff' auth user repo base headref =
    executeRequestMaybe auth $ diffR user repo base headref

-- | Compare two commits.
-- See <https://developer.github.com/v3/repos/commits/#compare-two-commits>
diffR :: Name Owner -> Name Repo -> Name Commit -> Name Commit -> Request k Diff
diffR user repo base headref =
    query ["repos", toPathPart user, toPathPart repo, "compare", toPathPart base <> "..." <> toPathPart headref] []
