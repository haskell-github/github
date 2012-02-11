module ListLanguages where

import qualified Github.Repos as Github
import Data.List
import Data.Default (def)

main = do
  possibleLanguages <- Github.languagesFor def "mike-burns" "ohlaunch"
  case possibleLanguages of
       (Left error) -> putStrLn $ "Error: " ++ (show error)
       (Right languages) ->
         putStrLn $ intercalate "\n" $ map formatLanguage languages

formatLanguage (Github.Language name characterCount) =
  name ++ "\t" ++ show characterCount
