-- |
-- The API for underlying git commits of a Github repo, as described on
-- <http://developer.github.com/v3/git/commits/>.

module GitHub.Endpoints.GitData.Commits (
    gitCommitR,
    module GitHub.Data,
) where

import GitHub.Data
import Prelude ()

-- | Query a commit.
-- See <https://developer.github.com/v3/git/commits/#get-a-commit>
gitCommitR :: Name Owner -> Name Repo -> Name GitCommit -> Request k GitCommit
gitCommitR user repo sha =
    query ["repos", toPathPart user, toPathPart repo, "git", "commits", toPathPart sha] []
