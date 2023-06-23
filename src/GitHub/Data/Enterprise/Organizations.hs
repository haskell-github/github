module GitHub.Data.Enterprise.Organizations where

import GitHub.Data.Definitions
import GitHub.Data.Name (Name)
import GitHub.Data.URL (URL)
import GitHub.Internal.Prelude
import Prelude ()

data CreateOrganization = CreateOrganization
    { createOrganizationLogin       :: !(Name Organization)
    , createOrganizationAdmin       :: !(Name User)
    , createOrganizationProfileName :: !(Maybe Text)
    }
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData CreateOrganization where rnf = genericRnf
instance Binary CreateOrganization

data RenameOrganization = RenameOrganization
    { renameOrganizationLogin :: !(Name Organization)
    }
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData RenameOrganization where rnf = genericRnf
instance Binary RenameOrganization

data RenameOrganizationResponse = RenameOrganizationResponse
    { renameOrganizationResponseMessage :: !Text
    , renameOrganizationResponseUrl     :: !URL
    }
    deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData RenameOrganizationResponse where rnf = genericRnf
instance Binary RenameOrganizationResponse

-- JSON Instances

instance ToJSON CreateOrganization where
    toJSON (CreateOrganization login admin profileName) =
        object $ filter notNull
            [ "login"        .= login
            , "admin"        .= admin
            , "profile_name" .= profileName
            ]
      where
        notNull (_, Null) = False
        notNull (_, _) = True

instance ToJSON RenameOrganization where
    toJSON (RenameOrganization login) =
        object
            [ "login" .= login
            ]

instance FromJSON RenameOrganizationResponse where
    parseJSON = withObject "RenameOrganizationResponse" $ \o ->
        RenameOrganizationResponse
            <$> o .: "message"
            <*> o .: "url"
