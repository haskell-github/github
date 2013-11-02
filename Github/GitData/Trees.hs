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
-- > tree "thoughtbot" "paperclip" "fe114451f7d066d367a1646ca7ac10e689b46844"
tree :: String -> String -> String -> IO (Either Error Tree)
tree user reqRepoName sha =
  githubGet ["repos", user, reqRepoName, "git", "trees", sha]

-- | A recursively-nested tree for a SHA1.
--
-- > nestedTree "thoughtbot" "paperclip" "fe114451f7d066d367a1646ca7ac10e689b46844"
nestedTree :: String -> String -> String -> IO (Either Error Tree)
nestedTree user reqRepoName sha =
  githubGetWithQueryString ["repos", user, reqRepoName, "git", "trees", sha]
                           "recursive=1"
