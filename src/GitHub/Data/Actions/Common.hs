-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
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

data WithTotalCount a = WithTotalCount
    { withTotalCountItems :: !(Vector a)
    , withTotalCountTotalCount :: !Int
    } deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance Semigroup (WithTotalCount a) where
    (WithTotalCount items1 count1) <> (WithTotalCount items2 _) =
        WithTotalCount (items1 <> items2) count1

instance Foldable WithTotalCount where
    foldMap f (WithTotalCount items _) = foldMap f items
