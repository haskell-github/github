-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}
module GitHub.Data.Actions.Common (
    PaginatedWithTotalCount(..),
    WithTotalCount(..),
    ) where


import GHC.TypeLits
import GitHub.Internal.Prelude
import Prelude ()

import           Data.Data (Proxy (..))
import qualified Data.Text as T

-------------------------------------------------------------------------------
-- Common
-------------------------------------------------------------------------------

data PaginatedWithTotalCount a (tag :: Symbol) = PaginatedWithTotalCount
    { paginatedWithTotalCountItems :: !(Vector a)
    , paginatedWithTotalCountTotalCount :: !Int
    }

data WithTotalCount a = WithTotalCount
    { withTotalCountItems :: !(Vector a)
    , withTotalCountTotalCount :: !Int
    } deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance Semigroup (WithTotalCount a) where
    (WithTotalCount items1 count1) <> (WithTotalCount items2 _) =
        WithTotalCount (items1 <> items2) count1

instance Foldable WithTotalCount where
    foldMap f (WithTotalCount items _) = foldMap f items

-------------------------------------------------------------------------------
-- JSON instances
-------------------------------------------------------------------------------


instance (FromJSON a, KnownSymbol l) => FromJSON (PaginatedWithTotalCount a l) where
    parseJSON = withObject "PaginatedWithTotalCount" $ \o -> PaginatedWithTotalCount
        <$> o .: T.pack (symbolVal (Proxy :: Proxy l))
        <*> o .: "total_count"
