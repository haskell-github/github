{-# LANGUAGE InstanceSigs #-}
module GitHub.Data.Reactions where

import qualified Data.Text as T
import GitHub.Data.Id (Id)
import GitHub.Data.Definitions (SimpleUser)
import GitHub.Internal.Prelude
import Prelude ()

data Reaction = Reaction
  { reactionId :: Id Reaction
  , reactionUser :: !(Maybe SimpleUser)
  , reactionContent :: !ReactionContent
  , reactionCreatedAt :: !UTCTime
  }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Reaction where rnf = genericRnf
instance Binary Reaction

data NewReaction = NewReaction
  { newReactionContent :: !ReactionContent
  }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData NewReaction where rnf = genericRnf
instance Binary NewReaction

-- |
-- <https://docs.github.com/en/rest/reactions/reactions?apiVersion=2022-11-28#about-reactions>
data ReactionContent
  = PlusOne
  | MinusOne
  | Laugh
  | Confused
  | Heart
  | Hooray
  | Rocket
  | Eyes
  deriving (Show, Data, Typeable, Eq, Ord, Enum, Bounded, Generic)

instance NFData ReactionContent where rnf = genericRnf
instance Binary ReactionContent

-- JSON instances

instance FromJSON Reaction where
  parseJSON = withObject "Reaction" $ \o ->
    Reaction
      <$> o .: "id"
      <*> o .:? "user"
      <*> o .: "content"
      <*> o .: "created_at"

instance ToJSON NewReaction where
  toJSON (NewReaction content) = object ["content" .= content]

instance FromJSON ReactionContent where
  parseJSON = withText "ReactionContent" $ \case
    "+1" -> pure PlusOne
    "-1" -> pure MinusOne
    "laugh" -> pure Laugh
    "confused" -> pure Confused
    "heart" -> pure Heart
    "hooray" -> pure Hooray
    "rocket" -> pure Rocket
    "eyes" -> pure Eyes
    t -> fail $ "Unknown ReactionContent: " <> T.unpack t

instance ToJSON ReactionContent where
  toJSON PlusOne = String "+1"
  toJSON MinusOne = String "-1"
  toJSON Laugh = String "laugh"
  toJSON Confused = String "confused"
  toJSON Heart = String "heart"
  toJSON Hooray = String "hooray"
  toJSON Rocket = String "rocket"
  toJSON Eyes = String "eyes"
