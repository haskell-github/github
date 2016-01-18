{-# LANGUAGE OverloadedStrings #-}
-- | The underlying tree of SHA1s and files that make up a git repo. The API is
-- described on <http://developer.github.com/v3/git/trees/>.
module Github.GitData.Trees (
    tree,
    tree',
    treeR,
    nestedTree,
    nestedTree',
    nestedTreeR,
    module Github.Data,
    ) where

import Github.Data
import Github.Request

-- | A tree for a SHA1.
--
-- > tree (Just ("github-username", "github-password")) "thoughtbot" "paperclip" "fe114451f7d066d367a1646ca7ac10e689b46844"
tree' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> Name Tree -> IO (Either Error Tree)
tree' auth user repo sha =
    executeRequestMaybe auth $ treeR user repo sha

-- | A tree for a SHA1.
--
-- > tree "thoughtbot" "paperclip" "fe114451f7d066d367a1646ca7ac10e689b46844"
tree :: Name GithubOwner -> Name Repo -> Name Tree -> IO (Either Error Tree)
tree = tree' Nothing

-- | Get a Tree.
-- See <https://developer.github.com/v3/git/trees/#get-a-tree>
treeR :: Name GithubOwner -> Name Repo -> Name Tree -> GithubRequest k Tree
treeR user repo sha =
    GithubGet  ["repos", toPathPart user, toPathPart repo, "git", "trees", toPathPart sha] []

-- | A recursively-nested tree for a SHA1.
--
-- > nestedTree' (Just ("github-username", "github-password")) "thoughtbot" "paperclip" "fe114451f7d066d367a1646ca7ac10e689b46844"
nestedTree' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> Name Tree -> IO (Either Error Tree)
nestedTree' auth user repo sha =
    executeRequestMaybe auth $ nestedTreeR user repo sha

-- | A recursively-nested tree for a SHA1.
--
-- > nestedTree "thoughtbot" "paperclip" "fe114451f7d066d367a1646ca7ac10e689b46844"
nestedTree :: Name GithubOwner -> Name Repo -> Name Tree -> IO (Either Error Tree)
nestedTree = nestedTree' Nothing

-- | Get a Tree Recursively.
-- See <https://developer.github.com/v3/git/trees/#get-a-tree-recursively>
nestedTreeR :: Name GithubOwner -> Name Repo -> Name Tree -> GithubRequest k Tree
nestedTreeR user repo sha =
    GithubGet  ["repos", toPathPart user, toPathPart repo, "git", "trees", toPathPart sha] [("recursive", Just "1")]
