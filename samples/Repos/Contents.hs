{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Common                          hiding
                 (getContents, intercalate, take, truncate, unlines)
import           Data.Text
                 (Text, intercalate, pack, take, unlines)
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

formatItem item =
   "type: " <> tshow (GitHub.contentItemType item) <> "\n" <>
  formatContentInfo (GitHub.contentItemInfo item)


truncate str = take 40 str <> "... (truncated)"
