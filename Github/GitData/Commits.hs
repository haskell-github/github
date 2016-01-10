-- | The API for underlying git commits of a Github repo, as described on
-- <http://developer.github.com/v3/git/commits/>.
module Github.GitData.Commits (
    commit,
    gitCommitR,
    module Github.Data,
) where

import Github.Data
import Github.Request

-- | A single commit, by SHA1.
--
-- > commit "thoughtbot" "paperclip" "bc5c51d1ece1ee45f94b056a0f5a1674d7e8cba9"
commit :: Name GithubOwner -> Name Repo -> Name GitCommit -> IO (Either Error GitCommit)
commit user repo sha =
    executeRequest' $ gitCommitR user repo sha

-- | Get a commit.
-- See <https://developer.github.com/v3/git/commits/#get-a-commit>
gitCommitR :: Name GithubOwner -> Name Repo -> Name GitCommit -> GithubRequest k GitCommit
gitCommitR user repo sha =
    GithubGet ["repos", toPathPart user, toPathPart repo, "git", "commits", toPathPart sha] []
