module ListGists where

import qualified Github.Gists as Github
import Data.List (intercalate)

main = do
  possibleGists <- Github.gists "mike-burns"
  case possibleGists of
    (Left error)  -> putStrLn $ "Error: " ++ (show error)
    (Right gists) -> putStrLn $ intercalate "\n\n" $ map formatGist gists

formatGist gist =
  (Github.gistId gist) ++ "\n" ++
    (maybe "indescribable" id $ Github.gistDescription gist) ++ "\n" ++
    (Github.gistHtmlUrl gist)
