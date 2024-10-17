{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- This module may change between minor releases. Do not rely on its contents.

module GitHub.Internal.Prelude ( module X ) where

import Control.Applicative      as X ((<|>))
import Control.DeepSeq          as X (NFData (..))
import Control.DeepSeq.Generics as X (genericRnf)
import Data.Aeson               as X
       (FromJSON (..), Object, ToJSON (..), Value (..), encode, object,
       withObject, withText, (.!=), (.:), (.:?), (.=))
import Data.Aeson.Types         as X (emptyObject, typeMismatch)
import Data.Binary              as X (Binary)
import Data.Binary.Instances    as X ()
import Data.Data                as X (Data, Typeable)
import Data.Foldable            as X (toList)
import Data.Hashable            as X (Hashable (..))
import Data.HashMap.Strict      as X (HashMap)
import Data.List                as X (intercalate)
import Data.Maybe               as X (catMaybes)
import Data.Semigroup           as X (Semigroup (..))
import Data.String              as X (IsString (..))
import Data.Text                as X (Text, pack, unpack)
import Data.Time                as X (UTCTime)
import Data.Time.ISO8601        as X (formatISO8601)
import Data.Vector              as X (Vector)
import GHC.Generics             as X (Generic)
import Prelude.Compat           as X
import Data.Functor.Compat      as X ((<&>))
