module ListLanguages where

import qualified Github.Repos as Github
import Data.List

main = do
  possibleLanguages <- Github.languagesFor "mike-burns" "ohlaunch"
  case possibleLanguages of
       (Left error) -> putStrLn $ "Error: " ++ (show error)
       (Right languages) ->
         putStrLn $ intercalate "\n" $ map formatLanguage languages

formatLanguage (Github.Language name characterCount) =
  name ++ "\t" ++ show characterCount
