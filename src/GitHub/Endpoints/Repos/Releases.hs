-- The Release API, as described at
-- <https://developer.github.com/v3/repos/releases/>.
module GitHub.Endpoints.Repos.Releases (
    releases,
    releases',
    releasesR,
    release,
    release',
    releaseR,
    latestRelease,
    latestRelease',
    latestReleaseR,
    releaseByTagName,
    releaseByTagName',
    releaseByTagNameR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

-- | All releases for the given repo.
--
-- > releases "calleerlandsson" "pick"
releases :: Name Owner -> Name Repo -> IO (Either Error  (Vector Release))
releases = releases' Nothing

-- | All releases for the given repo with authentication.
--
-- > releases' (Just (User (user, password))) "calleerlandsson" "pick"
releases' :: Maybe Auth -> Name Owner -> Name Repo -> IO (Either Error  (Vector Release))
releases' auth user repo =
    executeRequestMaybe auth $ releasesR user repo FetchAll

-- | List releases for a repository.
-- See <https://developer.github.com/v3/repos/releases/#list-releases-for-a-repository>
releasesR :: Name Owner -> Name Repo -> FetchCount -> Request k (Vector Release)
releasesR user repo =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "releases"] []

-- | Query a single release.
--
-- > release "calleerlandsson" "pick"
release :: Name Owner -> Name Repo -> Id Release -> IO (Either Error Release)
release = release' Nothing

-- | Query a single release with authentication.
--
-- > release' (Just (User (user, password))) "calleerlandsson" "pick"
release' :: Maybe Auth -> Name Owner -> Name Repo -> Id Release -> IO (Either Error Release)
release' auth user repo reqReleaseId =
    executeRequestMaybe auth $ releaseR user repo reqReleaseId

-- | Get a single release.
-- See <https://developer.github.com/v3/repos/releases/#get-a-single-release>
releaseR :: Name Owner -> Name Repo -> Id Release -> Request k Release
releaseR user repo reqReleaseId =
    query ["repos", toPathPart user, toPathPart repo, "releases", toPathPart reqReleaseId ] []

-- | Query latest release.
--
-- > latestRelease "calleerlandsson" "pick"
latestRelease :: Name Owner -> Name Repo -> IO (Either Error Release)
latestRelease = latestRelease' Nothing

-- | Query latest release with authentication.
--
-- > latestRelease' (Just (User (user, password))) "calleerlandsson" "pick"
latestRelease' :: Maybe Auth -> Name Owner -> Name Repo -> IO (Either Error Release)
latestRelease' auth user repo  =
    executeRequestMaybe auth $ latestReleaseR user repo

-- | Get the latest release.
-- See <https://developer.github.com/v3/repos/releases/#get-the-latest-release>
latestReleaseR :: Name Owner -> Name Repo -> Request k Release
latestReleaseR user repo =
    query ["repos", toPathPart user, toPathPart repo, "releases", "latest" ] []

-- | Query release by tag name.
--
-- > releaseByTagName "calleerlandsson" "pick"
releaseByTagName :: Name Owner -> Name Repo -> Text -> IO (Either Error Release)
releaseByTagName = releaseByTagName' Nothing

-- | Query release by tag name with authentication.
--
-- > releaseByTagName' (Just (User (user, password))) "calleerlandsson" "pick"
releaseByTagName' :: Maybe Auth -> Name Owner -> Name Repo -> Text -> IO (Either Error Release)
releaseByTagName' auth user repo reqTagName =
    executeRequestMaybe auth $ releaseByTagNameR user repo reqTagName

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
