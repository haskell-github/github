{-# LANGUAGE OverloadedStrings #-}
module CreateLabels where

import           Data.List            (intercalate)
import qualified Github.Auth          as Github
import qualified Github.Issues.Labels as Github
main = do
  let auth = Github.BasicAuth "user" "password"
  possibleLabel <- Github.createLabel auth "thoughtbot" "papperclip" "sample label" "ff00ff"
  case possibleLabel of
       (Left error) -> putStrLn $ "Error: " ++ show error
       (Right label) -> putStrLn . formatLabel $ label

formatLabel label = Github.labelName label ++
                    ", colored " ++
                    Github.labelColor label
