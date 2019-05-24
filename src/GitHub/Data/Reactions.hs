-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
--
module GitHub.Data.Reactions where

import GitHub.Data.URL         (URL)
import GitHub.Internal.Prelude
import Prelude ()

data Reactions = Reactions
    { reactionsUrl      :: !URL
    , reactionsCount    :: !Int
    , reactionsPlus1    :: !Int
    , reactionsMinus1   :: !Int
    , reactionsLaugh    :: !Int
    , reactionsHooray   :: !Int
    , reactionsConfused :: !Int
    , reactionsHeart    :: !Int
    , reactionsRocket   :: !Int
    , reactionsEyes     :: !Int
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance FromJSON Reactions where
    parseJSON = withObject "Reactions" $ \o -> Reactions
        <$> o .: "url"
        <*> o .:? "total_count" .!= 0
        <*> o .:? "+1"          .!= 0
        <*> o .:? "-1"          .!= 0
        <*> o .:? "laugh"       .!= 0
        <*> o .:? "hooray"      .!= 0
        <*> o .:? "confused"    .!= 0
        <*> o .:? "heart"       .!= 0
        <*> o .:? "rocket"      .!= 0
        <*> o .:? "eyes"        .!= 0

instance NFData Reactions where rnf = genericRnf
instance Binary Reactions
