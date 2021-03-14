{-# LANGUAGE OverloadedStrings#-}
module Main(main) where

import qualified GitHub.Endpoints.Repos.Projects as P
import Data.List
import GitHub.Data
import GitHub.Data.Name
import GitHub.Data.Request
import Common
import qualified GitHub
import Prelude ()

main = do
    auth <- getAuth
    possibleProjects <- GitHub.executeRequestMaybe auth $ P.repoProjectsForR "lambda-coast" "infinite-turtles"  GitHub.FetchAll
    putStrLn $ either (("Error: " <>) . tshow)
                      (foldMap ((<> "\n") . tshow))
                      possibleProjects


    possibleProjects <- GitHub.executeRequestMaybe auth $ P.orgProjectsForR "lambda-coast" GitHub.FetchAll
    putStrLn $ either (("Error: " <>) . tshow)
                      (foldMap ((<> "\n") . tshow))
                      possibleProjects
