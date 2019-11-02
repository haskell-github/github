-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The underlying tree of SHA1s and files that make up a git repo. The API is
-- described on <http://developer.github.com/v3/git/trees/>.
module GitHub.Endpoints.GitData.Trees (
    tree,
    tree',
    treeR,
    nestedTree,
    nestedTree',
    nestedTreeR,
    createTree,
    createTreeR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

-- | A tree for a SHA1.
--
-- > tree (Just $ BasicAuth "github-username" "github-password") "thoughtbot" "paperclip" "fe114451f7d066d367a1646ca7ac10e689b46844"
tree' :: Maybe Auth -> Name Owner -> Name Repo -> Name Tree -> IO (Either Error Tree)
tree' auth user repo sha =
    executeRequestMaybe auth $ treeR user repo sha

-- | A tree for a SHA1.
--
-- > tree "thoughtbot" "paperclip" "fe114451f7d066d367a1646ca7ac10e689b46844"
tree :: Name Owner -> Name Repo -> Name Tree -> IO (Either Error Tree)
tree = tree' Nothing

-- | Query a Tree.
-- See <https://developer.github.com/v3/git/trees/#get-a-tree>
treeR :: Name Owner -> Name Repo -> Name Tree -> Request k Tree
treeR user repo sha =
    query  ["repos", toPathPart user, toPathPart repo, "git", "trees", toPathPart sha] []

-- | A recursively-nested tree for a SHA1.
--
-- > nestedTree' (Just $ BasicAuth "github-username" "github-password") "thoughtbot" "paperclip" "fe114451f7d066d367a1646ca7ac10e689b46844"
nestedTree' :: Maybe Auth -> Name Owner -> Name Repo -> Name Tree -> IO (Either Error Tree)
nestedTree' auth user repo sha =
    executeRequestMaybe auth $ nestedTreeR user repo sha

-- | A recursively-nested tree for a SHA1.
--
-- > nestedTree "thoughtbot" "paperclip" "fe114451f7d066d367a1646ca7ac10e689b46844"
nestedTree :: Name Owner -> Name Repo -> Name Tree -> IO (Either Error Tree)
nestedTree = nestedTree' Nothing

-- | Query a Tree Recursively.
-- See <https://developer.github.com/v3/git/trees/#get-a-tree-recursively>
nestedTreeR :: Name Owner -> Name Repo -> Name Tree -> Request k Tree
nestedTreeR user repo sha =
    query  ["repos", toPathPart user, toPathPart repo, "git", "trees", toPathPart sha] [("recursive", Just "1")]

-- | Create a tree.
createTree :: Auth -> Name Owner -> Name Repo -> NewTree -> IO (Either Error Tree)
createTree auth user repo newTree =
    executeRequest auth $ createTreeR user repo newTree

-- | Create a tree.
-- See <https://developer.github.com/v3/git/refs/#create-a-reference>
createTreeR :: Name Owner -> Name Repo -> NewTree -> Request 'RW Tree
createTreeR user repo newTree =
     command Post  ["repos", toPathPart user, toPathPart repo , "git", "trees"] (encode newTree)