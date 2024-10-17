-- |
-- The repo commits API as described on
-- <http://developer.github.com/v3/repos/commits/>.

module GitHub.Endpoints.Repos.Commits (
    CommitQueryOption(..),
    commitsForR,
    commitsWithOptionsForR,
    commitR,
    diffR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
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

-- | List commits on a repository.
-- See <https://developer.github.com/v3/repos/commits/#list-commits-on-a-repository>
commitsForR :: Name Owner -> Name Repo -> FetchCount -> Request k (Vector Commit)
commitsForR user repo limit = commitsWithOptionsForR user repo limit []

-- | List commits on a repository.
-- See <https://developer.github.com/v3/repos/commits/#list-commits-on-a-repository>
commitsWithOptionsForR :: Name Owner -> Name Repo -> FetchCount -> [CommitQueryOption] -> Request k (Vector Commit)
commitsWithOptionsForR user repo limit opts =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "commits"] qs limit
  where
    qs = map renderCommitQueryOption opts

-- | Query a single commit.
-- See <https://developer.github.com/v3/repos/commits/#get-a-single-commit>
commitR :: Name Owner -> Name Repo -> Name Commit -> Request k Commit
commitR user repo sha =
    query ["repos", toPathPart user, toPathPart repo, "commits", toPathPart sha] []

-- | Compare two commits.
-- See <https://developer.github.com/v3/repos/commits/#compare-two-commits>
diffR :: Name Owner -> Name Repo -> Name Commit -> Name Commit -> Request k Diff
diffR user repo base headref =
    query ["repos", toPathPart user, toPathPart repo, "compare", toPathPart base <> "..." <> toPathPart headref] []
