{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified GitHub as GH
import Data.List (intercalate)
import Data.Vector (toList)

main :: IO ()
main = do
  let auth = GH.OAuth "auth_token"
  eDeployKeys <- GH.github auth GH.deployKeysForR "your_owner" "your_repo" GH.FetchAll
  case eDeployKeys of
    Left err         -> putStrLn $ "Error: " ++ show err
    Right deployKeys -> putStrLn $ intercalate "\n" $ map formatRepoDeployKey (toList deployKeys)

formatRepoDeployKey :: DK.RepoDeployKey -> String
formatRepoDeployKey = show

