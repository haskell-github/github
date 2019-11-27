-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The underlying tree of SHA1s and files that make up a git repo. The API is
-- described on <http://developer.github.com/v3/git/trees/>.
module GitHub.Endpoints.GitData.Trees (
    treeR,
    nestedTreeR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

-- | Query a Tree.
-- See <https://developer.github.com/v3/git/trees/#get-a-tree>
treeR :: Name Owner -> Name Repo -> Name Tree -> Request k Tree
treeR user repo sha =
    query  ["repos", toPathPart user, toPathPart repo, "git", "trees", toPathPart sha] []

-- | Query a Tree Recursively.
-- See <https://developer.github.com/v3/git/trees/#get-a-tree-recursively>
nestedTreeR :: Name Owner -> Name Repo -> Name Tree -> Request k Tree
nestedTreeR user repo sha =
    query  ["repos", toPathPart user, toPathPart repo, "git", "trees", toPathPart sha] [("recursive", Just "1")]
