-- | The API for underlying git commits of a Github repo, as described on
-- <http://developer.github.com/v3/git/commits/>.
module Github.GitData.Commits (
 commit
,module Github.Data
) where

import Github.Data
import Github.Private

-- | A single commit, by SHA1.
--
-- > commit "thoughtbot" "paperclip" "bc5c51d1ece1ee45f94b056a0f5a1674d7e8cba9"
commit :: GithubConfig -> String -> String -> String -> IO (Either Error GitCommit)
commit c user repoName sha =
  githubGet c ["repos", user, repoName, "git", "commits", sha]
