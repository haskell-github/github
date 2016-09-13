-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module GitHub.Data.Milestone where

import GitHub.Data.Definitions
import GitHub.Data.Id           (Id)
import GitHub.Data.URL          (URL)
import GitHub.Internal.Prelude
import Prelude ()

data Milestone = Milestone
    { milestoneCreator      :: !SimpleUser
    , milestoneDueOn        :: !(Maybe UTCTime)
    , milestoneOpenIssues   :: !Int
    , milestoneNumber       :: !(Id Milestone)
    , milestoneClosedIssues :: !Int
    , milestoneDescription  :: !(Maybe Text)
    , milestoneTitle        :: !Text
    , milestoneUrl          :: !URL
    , milestoneCreatedAt    :: !UTCTime
    , milestoneState        :: !Text
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Milestone where rnf = genericRnf
instance Binary Milestone

instance FromJSON Milestone where
    parseJSON = withObject "Milestone" $ \o -> Milestone
        <$> o .: "creator"
        <*> o .: "due_on"
        <*> o .: "open_issues"
        <*> o .: "number"
        <*> o .: "closed_issues"
        <*> o .: "description"
        <*> o .: "title"
        <*> o .: "url"
        <*> o .: "created_at"
        <*> o .: "state"
