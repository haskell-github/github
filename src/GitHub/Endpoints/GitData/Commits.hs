-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The API for underlying git commits of a Github repo, as described on
-- <http://developer.github.com/v3/git/commits/>.
module GitHub.Endpoints.GitData.Commits (
    commit,
    commit',
    gitCommitR,
    createCommit,
    createCommitR,
    module GitHub.Data,
) where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

-- | A single commit, by SHA1.
--

-- > reference' (Just $ BasicAuth "github-username" "github-password") "mike-burns" "github" "heads/master"
commit' :: Maybe Auth -> Name Owner -> Name Repo -> Name GitCommit -> IO (Either Error GitCommit)
commit' auth user repo sha =
    executeRequestMaybe auth $ gitCommitR user repo sha

-- > commit "thoughtbot" "paperclip" "bc5c51d1ece1ee45f94b056a0f5a1674d7e8cba9"
commit :: Name Owner -> Name Repo -> Name GitCommit -> IO (Either Error GitCommit)
commit =
    commit' Nothing

-- | Query a commit.
-- See <https://developer.github.com/v3/git/commits/#get-a-commit>
gitCommitR :: Name Owner -> Name Repo -> Name GitCommit -> Request k GitCommit
gitCommitR user repo sha =
    query ["repos", toPathPart user, toPathPart repo, "git", "commits", toPathPart sha] []

-- | Create a tree.
createCommit :: Auth -> Name Owner -> Name Repo -> NewGitCommit -> IO (Either Error GitCommit)
createCommit auth user repo newTree =
    executeRequest auth $ createCommitR user repo newTree

-- | Create a commit.
-- See <https://developer.github.com/v3/git/refs/#create-a-reference>
createCommitR :: Name Owner -> Name Repo -> NewGitCommit -> Request 'RW GitCommit
createCommitR user repo newCommit =
     command Post  ["repos", toPathPart user, toPathPart repo , "git", "commits"] (encode newCommit)