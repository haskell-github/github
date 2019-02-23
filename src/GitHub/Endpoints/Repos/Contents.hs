-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The Github Repo Contents API, as documented at
-- <https://developer.github.com/v3/repos/contents/>
module GitHub.Endpoints.Repos.Contents (
    -- * Querying contents
    contentsFor,
    contentsFor',
    contentsForR,
    readmeFor,
    readmeFor',
    readmeForR,
    archiveFor,
    archiveFor',
    archiveForR,

    -- ** Create
    createFile,
    createFileR,

    -- ** Update
    updateFile,
    updateFileR,

    -- ** Delete
    deleteFile,
    deleteFileR,

    module GitHub.Data
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

import Data.Maybe (maybeToList)
import qualified Data.Text.Encoding as TE
import Network.URI (URI)

-- | The contents of a file or directory in a repo, given the repo owner, name, and path to the file
--
-- > contentsFor "thoughtbot" "paperclip" "README.md"
contentsFor :: Name Owner -> Name Repo -> Text -> Maybe Text -> IO (Either Error Content)
contentsFor = contentsFor' Nothing

-- | The contents of a file or directory in a repo, given the repo owner, name, and path to the file
-- With Authentication
--
-- > contentsFor' (Just (BasicAuth (user, password))) "thoughtbot" "paperclip" "README.md" Nothing
contentsFor' :: Maybe Auth ->  Name Owner -> Name Repo -> Text -> Maybe Text -> IO (Either Error Content)
contentsFor' auth user repo path ref =
    executeRequestMaybe auth $ contentsForR user repo path ref

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

-- | The contents of a README file in a repo, given the repo owner and name
--
-- > readmeFor "thoughtbot" "paperclip"
readmeFor :: Name Owner -> Name Repo -> IO (Either Error Content)
readmeFor = readmeFor' Nothing

-- | The contents of a README file in a repo, given the repo owner and name
-- With Authentication
--
-- > readmeFor' (Just (BasicAuth (user, password))) "thoughtbot" "paperclip"
readmeFor' :: Maybe Auth -> Name Owner -> Name Repo -> IO (Either Error Content)
readmeFor' auth user repo =
    executeRequestMaybe auth $ readmeForR user repo

readmeForR :: Name Owner -> Name Repo -> Request k Content
readmeForR user repo =
    query ["repos", toPathPart user, toPathPart repo, "readme"] []

-- | The archive of a repo, given the repo owner, name, and archive type
--
-- > archiveFor "thoughtbot" "paperclip" ArchiveFormatTarball Nothing
archiveFor :: Name Owner -> Name Repo -> ArchiveFormat -> Maybe Text -> IO (Either Error URI)
archiveFor = archiveFor' Nothing

-- | The archive of a repo, given the repo owner, name, and archive type
-- With Authentication
--
-- > archiveFor' (Just (BasicAuth (user, password))) "thoughtbot" "paperclip" ArchiveFormatTarball Nothing
archiveFor' :: Maybe Auth ->  Name Owner -> Name Repo -> ArchiveFormat -> Maybe Text -> IO (Either Error URI)
archiveFor' auth user repo path ref =
    executeRequestMaybe auth $ archiveForR user repo path ref

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
createFile
    :: Auth
    -> Name Owner      -- ^ owner
    -> Name Repo       -- ^ repository name
    -> CreateFile
    -> IO (Either Error ContentResult)
createFile auth user repo body =
    executeRequest auth $ createFileR user repo body

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
updateFile
    :: Auth
    -> Name Owner      -- ^ owner
    -> Name Repo       -- ^ repository name
    -> UpdateFile
    -> IO (Either Error ContentResult)
updateFile auth user repo body =
    executeRequest auth $ updateFileR user repo body

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
deleteFile
    :: Auth
    -> Name Owner      -- ^ owner
    -> Name Repo       -- ^ repository name
    -> DeleteFile
    -> IO (Either Error ())
deleteFile auth user repo body =
    executeRequest auth $ deleteFileR user repo body

-- | Delete a file.
-- See <https://developer.github.com/v3/repos/contents/#delete-a-file>
deleteFileR
    :: Name Owner
    -> Name Repo
    -> DeleteFile
    -> Request 'RW ()
deleteFileR user repo body =
    command Delete ["repos", toPathPart user, toPathPart repo, "contents", deleteFilePath body] (encode body)
