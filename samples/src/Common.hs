{-# LANGUAGE NoImplicitPrelude  #-}
module Common (
    -- * Common stuff
    getAuth,
    tshow,
    -- * Re-exports
    putStrLn,
    getArgs,
    Proxy(..),
    module GitHub.Internal.Prelude,
    ) where

import GitHub.Internal.Prelude hiding (putStrLn)

import Data.Proxy         (Proxy (..))
import Data.Text.IO       (putStrLn)
import System.Environment (lookupEnv)
import System.Environment (getArgs)

import qualified Data.Text as T
import qualified GitHub

getAuth :: IO (Maybe (GitHub.Auth))
getAuth = do
    token <- lookupEnv "GITHUB_TOKEN"
    pure (GitHub.OAuth . fromString <$> token)

tshow :: Show a => a -> Text
tshow = T.pack . show
