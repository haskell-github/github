{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- This module may change between minor releases. Do not rely on its contents.
module GitHub.Internal.Prelude (
    module Prelude.Compat,
    -- * Commonly used types
    UTCTime,
    HashMap,
    Text, pack, unpack,
    Vector,
    -- * Commonly used typeclasses
    Binary,
    Data, Typeable,
    Generic,
    Hashable(..),
    IsString(..),
    NFData(..), genericRnf,
    Semigroup(..),
    -- * Aeson
    FromJSON(..), ToJSON(..), Value(..), Object,
    emptyObject,
    encode,
    withText, withObject, (.:), (.:?), (.!=), (.=), object, typeMismatch,
    -- * Control.Applicative
    (<|>),
    -- * Data.Maybe
    catMaybes,
    -- * Data.List
    intercalate, toList,
    -- * Data.Time.ISO8601
    formatISO8601,
    ) where

import Control.Applicative      ((<|>))
import Control.DeepSeq          (NFData (..))
import Control.DeepSeq.Generics (genericRnf)
import Data.Aeson
       (FromJSON (..), Object, ToJSON (..), Value (..), encode, object,
       withObject, withText, (.!=), (.:), (.:?), (.=))
import Data.Aeson.Types         (emptyObject, typeMismatch)
import Data.Binary              (Binary)
import Data.Binary.Instances ()
import Data.Data                (Data, Typeable)
import Data.Foldable            (toList)
import Data.Hashable            (Hashable (..))
import Data.HashMap.Strict      (HashMap)
import Data.List                (intercalate)
import Data.Maybe               (catMaybes)
import Data.Semigroup           (Semigroup (..))
import Data.String              (IsString (..))
import Data.Text                (Text, pack, unpack)
import Data.Time.Compat         (UTCTime)
import Data.Time.ISO8601        (formatISO8601)
import Data.Vector              (Vector)
import Data.Vector.Instances ()
import GHC.Generics             (Generic)
import "base-compat" Prelude.Compat
