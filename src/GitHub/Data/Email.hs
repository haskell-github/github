-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module GitHub.Data.Email where

import GitHub.Internal.Prelude
import Prelude ()

data EmailVisibility
    = EmailVisibilityPrivate
    | EmailVisibilityPublic
    deriving (Show, Data, Enum, Bounded, Typeable, Eq, Ord, Generic)

instance NFData EmailVisibility where rnf = genericRnf
instance Binary EmailVisibility

instance FromJSON EmailVisibility where
    parseJSON (String "private") = pure EmailVisibilityPrivate
    parseJSON (String "public")  = pure EmailVisibilityPublic
    parseJSON _ = fail "Could not build an EmailVisibility"

data Email = Email
    { emailAddress    :: !Text
    , emailVerified   :: !Bool
    , emailPrimary    :: !Bool
    , emailVisibility :: !(Maybe EmailVisibility)
    } deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Email where rnf = genericRnf
instance Binary Email

instance FromJSON Email where
    parseJSON = withObject "Email" $ \o -> Email
        <$> o .:  "email"
        <*> o .:  "verified"
        <*> o .:  "primary"
        <*> o .:? "visibility"
