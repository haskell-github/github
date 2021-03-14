{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The repo commits API as described on
-- <https://docs.github.com/en/rest/reference/projects>
module GitHub.Endpoints.Repos.Projects (
    projectsForR
    ) where

import GitHub.Data
import GitHub.Data.Request
import GitHub.Data.Projects
import GitHub.Internal.Prelude
import Prelude ()
import qualified GitHub as GH
import Data.Tagged (Tagged (..))

data Inertia

instance GH.PreviewAccept Inertia where
  previewContentType = Tagged "application/vnd.github.inertia-preview+json"

instance FromJSON a => GH.PreviewParseResponse Inertia a where
  previewParseResponse _ res = Tagged (GH.parseResponseJSON res)


-- | List projects for a repository
-- See <https ://docs.github.com/en/rest/reference/projects#list-repository-projects
projectsForR :: Name Owner -> Name Repo -> FetchCount -> GenRequest ('MtPreview Inertia) k (Vector Project)
projectsForR user repo =
    PagedQuery ["repos", toPathPart user, toPathPart repo, "projects"] []
