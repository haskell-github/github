module ShowGist where

import qualified Github.Gists as Github
import Data.List (intercalate)

main = do
  possibleGist <- Github.gist "23084"
  case possibleGist of
    (Left error)  -> putStrLn $ "Error: " ++ (show error)
    (Right gist) -> putStrLn $ formatGist gist

formatGist gist =
  (Github.gistId gist) ++ "\n" ++
    (maybe "indescribable" id $ Github.gistDescription gist) ++ "\n" ++
    (Github.gistHtmlUrl gist) ++ "\n\n" ++
    (intercalate "\n\n" $ map formatGistFile $ Github.gistFiles gist)

formatGistFile gistFile =
  (Github.gistFileFilename gistFile) ++ ":\n" ++
    maybe "[empty]" id (Github.gistFileContent gistFile)
