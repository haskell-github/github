-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module GitHub.Data.Search where

import GitHub.Data.Repos       (Repo)
import GitHub.Data.URL         (URL)
import GitHub.Internal.Prelude
import Prelude ()

import qualified Data.Vector as V

data SearchResult entity = SearchResult
    { searchResultTotalCount :: !Int
    , searchResultResults    :: !(Vector entity)
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData entity => NFData (SearchResult entity) where rnf = genericRnf
instance Binary entity => Binary (SearchResult entity)

instance FromJSON entity => FromJSON (SearchResult entity) where
    parseJSON = withObject "SearchResult" $ \o -> SearchResult
        <$> o .: "total_count"
        <*> o .:? "items" .!= V.empty

data Code = Code
    { codeName    :: !Text
    , codePath    :: !Text
    , codeSha     :: !Text
    , codeUrl     :: !URL
    , codeGitUrl  :: !URL
    , codeHtmlUrl :: !URL
    , codeRepo    :: !Repo
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Code where rnf = genericRnf
instance Binary Code

instance FromJSON Code where
    parseJSON = withObject "Code" $ \o -> Code
        <$> o .: "name"
        <*> o .: "path"
        <*> o .: "sha"
        <*> o .: "url"
        <*> o .: "git_url"
        <*> o .: "html_url"
        <*> o .: "repository"
