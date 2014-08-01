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
-- > tree (Just ("github-username", "github-password")) "thoughtbot" "paperclip" "fe114451f7d066d367a1646ca7ac10e689b46844"
tree' :: Maybe GithubAuth -> String -> String -> String -> IO (Either Error Tree)
tree' auth user reqRepoName sha =
  githubGet' auth ["repos", user, reqRepoName, "git", "trees", sha]

-- | A tree for a SHA1.
--
-- > tree "thoughtbot" "paperclip" "fe114451f7d066d367a1646ca7ac10e689b46844"
tree :: String -> String -> String -> IO (Either Error Tree)
tree = tree' Nothing

-- | A recursively-nested tree for a SHA1.
--
-- > nestedTree' (Just ("github-username", "github-password")) "thoughtbot" "paperclip" "fe114451f7d066d367a1646ca7ac10e689b46844"
nestedTree' :: Maybe GithubAuth -> String -> String -> String -> IO (Either Error Tree)
nestedTree' auth user reqRepoName sha =
  githubGetWithQueryString' auth ["repos", user, reqRepoName, "git", "trees", sha]
                                 "recursive=1"

-- | A recursively-nested tree for a SHA1.
--
-- > nestedTree "thoughtbot" "paperclip" "fe114451f7d066d367a1646ca7ac10e689b46844"
nestedTree :: String -> String -> String -> IO (Either Error Tree)
nestedTree = nestedTree' Nothing
