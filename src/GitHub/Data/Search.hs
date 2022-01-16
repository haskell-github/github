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

data SearchResult' entities = SearchResult
    { searchResultTotalCount :: !Int
    , searchResultResults    :: !entities
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

type SearchResult entity = SearchResult' (V.Vector entity)

instance NFData entities => NFData (SearchResult' entities) where rnf = genericRnf
instance Binary entities => Binary (SearchResult' entities)

instance (Monoid entities, FromJSON entities) => FromJSON (SearchResult' entities) where
    parseJSON = withObject "SearchResult" $ \o -> SearchResult
        <$> o .: "total_count"
        <*> o .:? "items" .!= mempty

instance Semigroup res => Semigroup (SearchResult' res) where
    (SearchResult count res) <> (SearchResult count' res') = SearchResult (max count count') (res <> res')

instance Foldable SearchResult' where
    foldMap f (SearchResult count results) = f results

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
