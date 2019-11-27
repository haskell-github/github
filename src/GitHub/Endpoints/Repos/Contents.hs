-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The Github Repo Contents API, as documented at
-- <https://developer.github.com/v3/repos/contents/>
module GitHub.Endpoints.Repos.Contents (
    -- * Querying contents
    contentsForR,
    readmeForR,
    archiveForR,

    -- ** Create
    createFileR,

    -- ** Update
    updateFileR,

    -- ** Delete
    deleteFileR,

    module GitHub.Data
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

import Data.Maybe (maybeToList)
import qualified Data.Text.Encoding as TE
import Network.URI (URI)

contentsForR
    :: Name Owner
    -> Name Repo
    -> Text            -- ^ file or directory
    -> Maybe Text      -- ^ Git commit
    -> Request k Content
contentsForR user repo path ref =
    query ["repos", toPathPart user, toPathPart repo, "contents", path] qs
  where
    qs =  maybe [] (\r -> [("ref", Just . TE.encodeUtf8 $ r)]) ref

readmeForR :: Name Owner -> Name Repo -> Request k Content
readmeForR user repo =
    query ["repos", toPathPart user, toPathPart repo, "readme"] []

-- | Get archive link.
-- See <https://developer.github.com/v3/repos/contents/#get-archive-link>
archiveForR
    :: Name Owner
    -> Name Repo
    -> ArchiveFormat   -- ^ The type of archive to retrieve
    -> Maybe Text      -- ^ Git commit
    -> GenRequest 'MtRedirect rw URI
archiveForR user repo format ref = Query path []
  where
    path = ["repos", toPathPart user, toPathPart repo, toPathPart format] <> maybeToList ref

-- | Create a file.
-- See <https://developer.github.com/v3/repos/contents/#create-a-file>
createFileR
    :: Name Owner
    -> Name Repo
    -> CreateFile
    -> Request 'RW ContentResult
createFileR user repo body =
    command Put ["repos", toPathPart user, toPathPart repo, "contents", createFilePath body] (encode body)

-- | Update a file.
-- See <https://developer.github.com/v3/repos/contents/#update-a-file>
updateFileR
    :: Name Owner
    -> Name Repo
    -> UpdateFile
    -> Request 'RW ContentResult
updateFileR user repo body =
    command Put ["repos", toPathPart user, toPathPart repo, "contents", updateFilePath body] (encode body)

-- | Delete a file.
-- See <https://developer.github.com/v3/repos/contents/#delete-a-file>
deleteFileR
    :: Name Owner
    -> Name Repo
    -> DeleteFile
    -> GenRequest 'MtUnit 'RW ()
deleteFileR user repo body =
    Command Delete ["repos", toPathPart user, toPathPart repo, "contents", deleteFilePath body] (encode body)
