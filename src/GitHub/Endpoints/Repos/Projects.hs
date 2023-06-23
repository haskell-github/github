{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The repo commits API as described on
-- <https://docs.github.com/en/rest/reference/projects>
module GitHub.Endpoints.Repos.Projects (
    repoProjectsForR
  , orgProjectsForR
  , projectColumnsForR
  , columnCardsForR
  ) where

import GitHub.Data
import GitHub.Data.Request
import GitHub.Request
import GitHub.Data.Projects
import GitHub.Internal.Prelude
import Prelude ()

-- | List projects for a repository
-- See <https ://docs.github.com/en/rest/reference/projects#list-repository-projects
repoProjectsForR :: Name Owner -> Name Repo -> FetchCount -> GenRequest ('MtPreview Inertia) k (Vector Project)
repoProjectsForR user repo =
    PagedQuery ["repos", toPathPart user, toPathPart repo, "projects"] []


orgProjectsForR :: Name Owner -> FetchCount -> GenRequest ( 'MtPreview Inertia) k (Vector Project)
orgProjectsForR user =
  PagedQuery ["orgs", toPathPart user, "projects"] []


projectColumnsForR :: (Id Project) -> FetchCount -> GenRequest ( 'MtPreview Inertia) k (Vector Column)
projectColumnsForR project_id =
  PagedQuery ["projects", toPathPart project_id, "columns"] []


columnCardsForR :: (Id Column) -> FetchCount -> GenRequest ( 'MtPreview Inertia) k (Vector Card)
columnCardsForR column_id =
  PagedQuery ["projects", "columns", toPathPart column_id, "cards"] []
