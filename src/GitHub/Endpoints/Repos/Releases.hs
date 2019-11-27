-- The Release API, as described at
-- <https://developer.github.com/v3/repos/releases/>.
module GitHub.Endpoints.Repos.Releases (
    releasesR,
    releaseR,
    latestReleaseR,
    releaseByTagNameR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

-- | List releases for a repository.
-- See <https://developer.github.com/v3/repos/releases/#list-releases-for-a-repository>
releasesR :: Name Owner -> Name Repo -> FetchCount -> Request k (Vector Release)
releasesR user repo =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "releases"] []

-- | Get a single release.
-- See <https://developer.github.com/v3/repos/releases/#get-a-single-release>
releaseR :: Name Owner -> Name Repo -> Id Release -> Request k Release
releaseR user repo reqReleaseId =
    query ["repos", toPathPart user, toPathPart repo, "releases", toPathPart reqReleaseId ] []

-- | Get the latest release.
-- See <https://developer.github.com/v3/repos/releases/#get-the-latest-release>
latestReleaseR :: Name Owner -> Name Repo -> Request k Release
latestReleaseR user repo =
    query ["repos", toPathPart user, toPathPart repo, "releases", "latest" ] []

-- | Get a release by tag name
-- See <https://developer.github.com/v3/repos/releases/#get-a-release-by-tag-name>
releaseByTagNameR :: Name Owner -> Name Repo -> Text -> Request k Release
releaseByTagNameR user repo reqTagName =
    query ["repos", toPathPart user, toPathPart repo, "releases", "tags" , reqTagName ] []

{-
--  TODO: implement the following:
    https://developer.github.com/v3/repos/releases/#create-a-release
    https://developer.github.com/v3/repos/releases/#edit-a-release
    https://developer.github.com/v3/repos/releases/#delete-a-release
    https://developer.github.com/v3/repos/releases/#list-assets-for-a-release
    https://developer.github.com/v3/repos/releases/#upload-a-release-asset
    https://developer.github.com/v3/repos/releases/#get-a-single-release-asset
    https://developer.github.com/v3/repos/releases/#edit-a-release-asset
    https://developer.github.com/v3/repos/releases/#delete-a-release-asset
-}
