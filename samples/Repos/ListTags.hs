module ListTags where

import qualified Github.Repos as Github
import Data.List

main = do
  possibleTags <- Github.tagsFor "thoughtbot" "paperclip"
  case possibleTags of
       (Left error) -> putStrLn $ "Error: " ++ (show error)
       (Right tags) ->
         putStrLn $ intercalate "\n" $ map Github.tagName tags
