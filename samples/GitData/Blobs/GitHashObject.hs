module GitHashObject where

import qualified Github.GitData.Blobs as Github
import Data.List( intercalate)

main = do
  possibleBlob <- Github.blob "thoughtbot" "paperclip" "bc5c51d1ece1ee45f94b056a0f5a1674d7e8cba9"
  case possibleBlob of
    (Left error) -> putStrLn $ "Error: " ++ (show error)
    (Right blob) -> putStrLn $ Github.blobContent blob
