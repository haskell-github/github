module GitHub.Data.Releases where

import GitHub.Data.Definitions
import GitHub.Data.Id          (Id)
import GitHub.Data.URL         (URL)
import GitHub.Internal.Prelude
import Prelude ()

data Release = Release
    { releaseUrl             :: !URL
    , releaseHtmlUrl         :: !URL
    , releaseAssetsurl       :: !URL
    , releaseUploadUrl       :: !URL
    , releaseTarballUrl      :: !URL
    , releaseZipballUrl      :: !URL
    , releaseId              :: !(Id Release)
    , releaseTagName         :: !Text
    , releaseTargetCommitish :: !Text
    , releaseName            :: !Text
    , releaseBody            :: !Text
    , releaseDraft           :: !Bool
    , releasePrerelease      :: !Bool
    , releaseCreatedAt       :: !UTCTime
    , releasePublishedAt     :: !(Maybe UTCTime)
    , releaseAuthor          :: !SimpleUser
    , releaseAssets          :: !(Vector ReleaseAsset)
    }
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance FromJSON Release where
    parseJSON = withObject "Event" $ \o -> Release
        <$> o .: "url"
        <*> o .: "html_url"
        <*> o .: "assets_url"
        <*> o .: "upload_url"
        <*> o .: "tarball_url"
        <*> o .: "zipball_url"
        <*> o .: "id"
        <*> o .: "tag_name"
        <*> o .: "target_commitish"
        <*> o .: "name"
        <*> o .: "body"
        <*> o .: "draft"
        <*> o .: "prerelease"
        <*> o .: "created_at"
        <*> o .:? "published_at"
        <*> o .: "author"
        <*> o .: "assets"

instance NFData Release where rnf = genericRnf
instance Binary Release

data ReleaseAsset = ReleaseAsset
    { releaseAssetUrl                :: !URL
    , releaseAssetBrowserDownloadUrl :: !Text
    , releaseAssetId                 :: !(Id ReleaseAsset)
    , releaseAssetName               :: !Text
    , releaseAssetLabel              :: !(Maybe Text)
    , releaseAssetState              :: !Text
    , releaseAssetContentType        :: !Text
    , releaseAssetSize               :: !Int
    , releaseAssetDownloadCount      :: !Int
    , releaseAssetCreatedAt          :: !UTCTime
    , releaseAssetUpdatedAt          :: !UTCTime
    , releaseAssetUploader           :: !SimpleUser
    }
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance FromJSON ReleaseAsset where
    parseJSON = withObject "Event" $ \o -> ReleaseAsset
        <$> o .: "url"
        <*> o .: "browser_download_url"
        <*> o .: "id"
        <*> o .: "name"
        <*> o .:? "label"
        <*> o .: "state"
        <*> o .: "content_type"
        <*> o .: "size"
        <*> o .: "download_count"
        <*> o .: "created_at"
        <*> o .: "updated_at"
        <*> o .: "uploader"

instance NFData ReleaseAsset where rnf = genericRnf
instance Binary ReleaseAsset
