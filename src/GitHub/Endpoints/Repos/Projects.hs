{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The repo commits API as described on
-- <http://developer.github.com/v3/repos/comments/>.
module GitHub.Endpoints.Repos.Projects (
    projectsForR
    ) where

import GitHub.Data
import GitHub.Data.Projects
import GitHub.Internal.Prelude
import Prelude ()

-- | List projects for a repository
-- See <https ://docs.github.com/en/rest/reference/projects#list-repository-projects
projectsForR :: Name Owner -> Name Repo -> FetchCount -> Request k (Vector Project)
projectsForR user repo =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "projects"] []
