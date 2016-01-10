module Common (
    -- * Common stuff
    getAuth,
    tshow,
    -- * Re-exports
    (<>),
    Text,
    putStrLn,
    module Prelude.Compat,
    ) where

import Prelude        ()
import Prelude.Compat hiding (putStrLn)

import Data.Monoid        ((<>))
import Data.Text          (Text)
import Data.Text.IO       (putStrLn)
import System.Environment (lookupEnv)

import qualified Data.Text   as T
import qualified Github.Data as Github

getAuth :: IO (Maybe (Github.GithubAuth))
getAuth = do
    token <- lookupEnv "GITHUB_TOKEN"
    pure (Github.GithubOAuth <$> token)

tshow :: Show a => a -> Text
tshow = T.pack . show
