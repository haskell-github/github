{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures    #-}

module GitHub.Data.Actions.Common (
    WithTotalCount(..),
    ) where

import GitHub.Internal.Prelude
import Prelude ()

-------------------------------------------------------------------------------
-- Common
-------------------------------------------------------------------------------

-- | A page of a paginated response.
data WithTotalCount a = WithTotalCount
    { withTotalCountItems      :: !(Vector a)
        -- ^ A snippet of the answer.
    , withTotalCountTotalCount :: !Int
        -- ^ The total size of the answer.
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

-- | Joining two pages of a paginated response.
--   The 'withTotalCountTotalCount' is assumed to be the same in both pages,
--   but this is not checked.
instance Semigroup (WithTotalCount a) where
    WithTotalCount items1 count1 <> WithTotalCount items2 _ =
        WithTotalCount (items1 <> items2) count1

instance Foldable WithTotalCount where
    foldMap f (WithTotalCount items _) = foldMap f items
