module GetReadme where

import qualified Github.Repos as Github
import Data.List
import Data.Maybe

main = do
  possibleReadme <- Github.readmeFor "jwiegley" "github"
  case possibleReadme of
       (Left error) -> putStrLn $ "Error: " ++ (show error)
       (Right (Github.ContentFile cd)) -> putStrLn $ (show cd)
