module Github.Events (
 parseEvent
) where

import qualified Data.ByteString.Lazy.Char8 as LBS

import Data.Aeson (FromJSON)

import Github.Data.Definitions (Error (..))
import Github.Private          (parseJson)

parseEvent :: (FromJSON b, Show b) => LBS.ByteString -> Either Error b
parseEvent = parseJson
