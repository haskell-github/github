-- | The underlying tree of SHA1s and files that make up a git repo. The API is
-- described on <http://developer.github.com/v3/git/trees/>.
module Github.GitData.Trees (
 tree
,nestedTree
,module Github.Data
) where

import Github.Data
import Github.Private

-- | A tree for a SHA1.
--
-- > tree def "thoughtbot" "paperclip" "fe114451f7d066d367a1646ca7ac10e689b46844"
tree :: GithubConfig -> String -> String -> String -> IO (Either Error Tree)
tree c user repoName sha =
  githubGet c ["repos", user, repoName, "git", "trees", sha]

-- | A recursively-nested tree for a SHA1.
--
-- > nestedTree def "thoughtbot" "paperclip" "fe114451f7d066d367a1646ca7ac10e689b46844"
nestedTree :: GithubConfig -> String -> String -> String -> IO (Either Error Tree)
nestedTree c user repoName sha =
  githubGetWithQueryString c ["repos", user, repoName, "git", "trees", sha]
                             "recursive=1"
