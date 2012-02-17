module GitHashObject where

import qualified Github.GitData.Blobs as Github
import Data.List( intercalate)

main = do
  possibleBlob <- Github.blob "mike-burns" "github" "1dc7b1f6e0c7bf1118f3b03195071dd6ea6db9b3"
  case possibleBlob of
    (Left error) -> putStrLn $ "Error: " ++ (show error)
    (Right blob) -> putStrLn $ Github.blobContent blob
