{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Common                          hiding
                 (getContents, intercalate, take, truncate, unlines)
import qualified Data.ByteString.Base64          as Base64
import           Data.Text
                 (Text, intercalate, take, unlines)
import           Data.Text.Encoding              (decodeUtf8, encodeUtf8)
import           Data.Text.IO                    (putStrLn)
import qualified Data.Vector                     as Vector
import qualified GitHub.Data                     as GitHub
import qualified GitHub.Endpoints.Repos.Contents as GitHub

main :: IO ()
main = do
  putStrLn "Root"
  putStrLn "===="
  getContents ""

  putStrLn "LICENSE"
  putStrLn "======="
  getContents "LICENSE"

  createUpdateDeleteSampleFile

getContents :: Text -> IO ()
getContents path = do
  contents <- GitHub.contentsFor "mike-burns" "ohlaunch" path Nothing
  putStrLn $ either (("Error: " <>) . tshow) formatContents contents

formatContents :: GitHub.Content -> Text
formatContents (GitHub.ContentFile fileData) =
  formatContentInfo (GitHub.contentFileInfo fileData) <>
  unlines
    [ tshow (GitHub.contentFileSize fileData) <> " bytes"
    , "encoding: " <> GitHub.contentFileEncoding fileData
    , "data: " <> truncate (GitHub.contentFileContent fileData)
    ]

formatContents (GitHub.ContentDirectory items) =
  intercalate "\n\n" . map formatItem . Vector.toList $ items

formatContentInfo :: GitHub.ContentInfo -> Text
formatContentInfo contentInfo =
  unlines
    [ "name: " <> GitHub.contentName contentInfo
    , "path: " <> GitHub.contentPath contentInfo
    , "sha: " <> GitHub.contentSha contentInfo
    , "url: " <> (GitHub.getUrl . GitHub.contentUrl) contentInfo
    , "git url: " <> (GitHub.getUrl . GitHub.contentGitUrl) contentInfo
    , "html url: " <> (GitHub.getUrl . GitHub.contentHtmlUrl) contentInfo
    ]

formatItem :: GitHub.ContentItem -> Text
formatItem item =
   "type: " <> tshow (GitHub.contentItemType item) <> "\n" <>
  formatContentInfo (GitHub.contentItemInfo item)

truncate :: Text -> Text
truncate str = take 40 str <> "... (truncated)"

createUpdateDeleteSampleFile :: IO ()
createUpdateDeleteSampleFile = do
  let
    auth = GitHub.OAuth "oauthtoken"
    owner = "repoOwner"
    repo = "repoName"
    author = GitHub.Author
      { GitHub.authorName = "John Doe"
      , GitHub.authorEmail = "johndoe@example.com"
      }
    defaultBranch = Nothing
    base64Encode = decodeUtf8 . Base64.encode . encodeUtf8
  createResult <- failOnError $ GitHub.createFile auth owner repo
    GitHub.CreateFile
    { GitHub.createFilePath      = "sample.txt"
    , GitHub.createFileMessage   = "Add sample.txt"
    , GitHub.createFileContent   = base64Encode "Hello"
    , GitHub.createFileBranch    = defaultBranch
    , GitHub.createFileAuthor    = Just author
    , GitHub.createFileCommitter = Just author
    }

  let getResultSHA = GitHub.contentSha . GitHub.contentResultInfo . GitHub.contentResultContent
  let createFileSHA = getResultSHA createResult
  updateResult <- failOnError $ GitHub.updateFile auth owner repo
    GitHub.UpdateFile
    { GitHub.updateFilePath      = "sample.txt"
    , GitHub.updateFileMessage   = "Update sample.txt"
    , GitHub.updateFileContent   = base64Encode "Hello world!"
    , GitHub.updateFileSHA       = createFileSHA
    , GitHub.updateFileBranch    = defaultBranch
    , GitHub.updateFileAuthor    = Just author
    , GitHub.updateFileCommitter = Just author
    }

  let updateFileSHA = getResultSHA updateResult
  failOnError $ GitHub.deleteFile auth owner repo
    GitHub.DeleteFile
    { GitHub.deleteFilePath      = "sample.txt"
    , GitHub.deleteFileMessage   = "Delete sample.txt"
    , GitHub.deleteFileSHA       = updateFileSHA
    , GitHub.deleteFileBranch    = defaultBranch
    , GitHub.deleteFileAuthor    = Just author
    , GitHub.deleteFileCommitter = Just author
    }

failOnError :: IO (Either GitHub.Error a) -> IO a
failOnError c = c >>= go
  where
  go r = case r of
    Left err -> fail . show $ err
    Right x -> return x
