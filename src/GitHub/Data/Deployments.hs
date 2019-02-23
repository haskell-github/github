{-# LANGUAGE LambdaCase #-}

module GitHub.Data.Deployments
    ( DeploymentQueryOption (..)
    , renderDeploymentQueryOption

    , Deployment (..)
    , CreateDeployment (..)

    , DeploymentStatus (..)
    , DeploymentStatusState (..)
    , CreateDeploymentStatus (..)
    ) where

import Control.Arrow (second)

import Data.ByteString (ByteString)
import Data.Maybe      (catMaybes)
import Data.Text       (Text)
import Data.Time.Clock (UTCTime)
import Data.Vector     (Vector)

import GitHub.Data.Definitions (SimpleUser)
import GitHub.Data.Id          (Id)
import GitHub.Data.Name        (Name)
import GitHub.Data.URL         (URL)
import GitHub.Internal.Prelude

import qualified Data.Aeson         as JSON
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text

data DeploymentQueryOption
    = DeploymentQuerySha         !Text
    | DeploymentQueryRef         !Text
    | DeploymentQueryTask        !Text
    | DeploymentQueryEnvironment !Text
      deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData DeploymentQueryOption where rnf = genericRnf
instance Binary DeploymentQueryOption

renderDeploymentQueryOption :: DeploymentQueryOption -> (ByteString, ByteString)
renderDeploymentQueryOption =
    second Text.encodeUtf8 . \case
        DeploymentQuerySha         sha  -> ("sha",         sha)
        DeploymentQueryRef         ref  -> ("ref",         ref)
        DeploymentQueryTask        task -> ("task",        task)
        DeploymentQueryEnvironment env  -> ("environment", env)

data Deployment a = Deployment
    { deploymentUrl           :: !URL
    , deploymentId            :: !(Id   (Deployment a))
    , deploymentSha           :: !(Name (Deployment a))
    , deploymentRef           :: !Text
    , deploymentTask          :: !Text
    , deploymentPayload       :: !(Maybe a)
    , deploymentEnvironment   :: !Text
    , deploymentDescription   :: !Text
    , deploymentCreator       :: !SimpleUser
    , deploymentCreatedAt     :: !UTCTime
    , deploymentUpdatedAt     :: !UTCTime
    , deploymentStatusesUrl   :: !URL
    , deploymentRepositoryUrl :: !URL
    } deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData a => NFData (Deployment a) where rnf = genericRnf
instance Binary a => Binary (Deployment a)

instance FromJSON a => FromJSON (Deployment a) where
    parseJSON = withObject "GitHub Deployment" $ \o ->
        Deployment
            <$> o .: "url"
            <*> o .: "id"
            <*> o .: "sha"
            <*> o .: "ref"
            <*> o .: "task"
            <*> o .:? "payload"
            <*> o .: "environment"
            <*> o .: "description"
            <*> o .: "creator"
            <*> o .: "created_at"
            <*> o .: "updated_at"
            <*> o .: "statuses_url"
            <*> o .: "repository_url"

data CreateDeployment a = CreateDeployment
    { createDeploymentRef              :: !Text
    -- ^ Required. The ref to deploy. This can be a branch, tag, or SHA.
    , createDeploymentTask             :: !(Maybe Text)
    -- ^ Specifies a task to execute (e.g., deploy or deploy:migrations).
    -- Default: deploy
    , createDeploymentAutoMerge        :: !(Maybe Bool)
    -- ^ Attempts to automatically merge the default branch into the requested
    -- ref, if it is behind the default branch. Default: true
    , createDeploymentRequiredContexts :: !(Maybe (Vector Text))
    -- ^ The status contexts to verify against commit status checks. If this
    -- parameter is omitted, then all unique contexts will be verified before a
    -- deployment is created. To bypass checking entirely pass an empty array.
    -- Defaults to all unique contexts.
    , createDeploymentPayload          :: !(Maybe a)
    -- ^ JSON payload with extra information about the deployment. Default: ""
    , createDeploymentEnvironment      :: !(Maybe Text)
    -- ^ Name for the target deployment environment (e.g., production, staging,
    -- qa). Default: production
    , createDeploymentDescription      :: !(Maybe Text)
    -- ^ Short description of the deployment. Default: ""
    } deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData a => NFData (CreateDeployment a) where rnf = genericRnf
instance Binary a => Binary (CreateDeployment a)

instance ToJSON a => ToJSON (CreateDeployment a) where
    toJSON x =
        JSON.object $ catMaybes
            [ Just ("ref"          .=      createDeploymentRef x)
            , ("task"              .=) <$> createDeploymentTask x
            , ("auto_merge"        .=) <$> createDeploymentAutoMerge x
            , ("required_contexts" .=) <$> createDeploymentRequiredContexts x
            , ("payload"           .=) <$> createDeploymentPayload x
            , ("environment"       .=) <$> createDeploymentEnvironment x
            , ("description"       .=) <$> createDeploymentDescription x
            ]

data DeploymentStatus = DeploymentStatus
    { deploymentStatusUrl           :: !URL
    , deploymentStatusId            :: !(Id DeploymentStatus)
    , deploymentStatusState         :: !DeploymentStatusState
    , deploymentStatusCreator       :: !SimpleUser
    , deploymentStatusDescription   :: !Text
    , deploymentStatusTargetUrl     :: !URL
    , deploymentStatusCreatedAt     :: !UTCTime
    , deploymentStatusUpdatedAt     :: !UTCTime
    , deploymentStatusDeploymentUrl :: !URL
    , deploymentStatusRepositoryUrl :: !URL
    } deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData DeploymentStatus where rnf = genericRnf
instance Binary DeploymentStatus

instance FromJSON DeploymentStatus where
    parseJSON = withObject "GitHub DeploymentStatus" $ \o ->
        DeploymentStatus
            <$> o .: "url"
            <*> o .: "id"
            <*> o .: "state"
            <*> o .: "creator"
            <*> o .: "description"
            <*> o .: "target_url"
            <*> o .: "created_at"
            <*> o .: "updated_at"
            <*> o .: "deployment_url"
            <*> o .: "repository_url"

data DeploymentStatusState
    = DeploymentStatusError
    | DeploymentStatusFailure
    | DeploymentStatusPending
    | DeploymentStatusSuccess
    | DeploymentStatusInactive
      deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData DeploymentStatusState where rnf = genericRnf
instance Binary DeploymentStatusState

instance ToJSON DeploymentStatusState where
    toJSON = \case
        DeploymentStatusError    -> "error"
        DeploymentStatusFailure  -> "failure"
        DeploymentStatusPending  -> "pending"
        DeploymentStatusSuccess  -> "success"
        DeploymentStatusInactive -> "inactive"

instance FromJSON DeploymentStatusState where
    parseJSON = withText "GitHub DeploymentStatusState" $ \case
        "error"    -> pure DeploymentStatusError
        "failure"  -> pure DeploymentStatusFailure
        "pending"  -> pure DeploymentStatusPending
        "success"  -> pure DeploymentStatusSuccess
        "inactive" -> pure DeploymentStatusInactive
        x          -> fail $ "Unknown deployment status: " ++ Text.unpack x

data CreateDeploymentStatus = CreateDeploymentStatus
    { createDeploymentStatusState       :: !DeploymentStatusState
    -- ^ Required. The state of the status. Can be one of error, failure,
    -- pending, or success.
    , createDeploymentStatusTargetUrl   :: !(Maybe Text) -- TODO: should this be URL?
    -- ^ The target URL to associate with this status. This URL should contain
    -- output to keep the user updated while the task is running or serve as
    -- historical information for what happened in the deployment. Default: ""
    , createDeploymentStatusDescription :: !(Maybe Text)
    -- ^ A short description of the status. Maximum length of 140 characters.
    -- Default: ""
    } deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData CreateDeploymentStatus where rnf = genericRnf
instance Binary CreateDeploymentStatus

instance ToJSON CreateDeploymentStatus where
    toJSON x =
        JSON.object $ catMaybes
            [ Just ("state"  .= createDeploymentStatusState x)
            , ("target_url"  .=) <$> createDeploymentStatusTargetUrl x
            , ("description" .=) <$> createDeploymentStatusDescription x
            ]
