module GetContents where

import qualified Github.Repos as Github
import Data.List
import Prelude hiding (truncate, getContents)

main = do
  putStrLn "Root"
  putStrLn "===="
  getContents ""

  putStrLn "LICENSE"
  putStrLn "======="
  getContents "LICENSE"

getContents path = do
  contents <- Github.contentsFor "mike-burns" "ohlaunch" path Nothing
  putStrLn $ either (("Error: " ++) . show) formatContents contents

formatContents (Github.ContentFile fileData) =
  formatContentInfo (Github.contentFileInfo fileData) ++
  unlines
    [ show (Github.contentFileSize fileData) ++ " bytes"
    , "encoding: " ++ Github.contentFileEncoding fileData
    , "data: " ++ truncate (Github.contentFileContent fileData)
    ]

formatContents (Github.ContentDirectory items) =
  intercalate "\n\n" $ map formatItem items

formatContentInfo contentInfo =
  unlines
    [ "name: " ++ Github.contentName contentInfo
    , "path: " ++ Github.contentPath contentInfo
    , "sha: " ++ Github.contentSha contentInfo
    , "url: " ++ Github.contentUrl contentInfo
    , "git url: " ++ Github.contentGitUrl contentInfo
    , "html url: " ++ Github.contentHtmlUrl contentInfo
    ]

formatItem item =
   "type: " ++ show (Github.contentItemType item) ++ "\n" ++
  formatContentInfo (Github.contentItemInfo item)


truncate str = take 40 str ++ "... (truncated)"
