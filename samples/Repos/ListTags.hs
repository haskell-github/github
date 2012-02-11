module ListTags where

import qualified Github.Repos as Github
import Data.List
import Data.Default (def)

main = do
  possibleTags <- Github.tagsFor def "thoughtbot" "paperclip"
  case possibleTags of
       (Left error) -> putStrLn $ "Error: " ++ (show error)
       (Right tags) ->
         putStrLn $ intercalate "\n" $ map Github.tagName tags
