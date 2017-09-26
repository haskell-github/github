{-# LANGUAGE OverloadedStrings #-}
module StarRepo where

import qualified GitHub.Endpoints.Activity.Starring as GH

import qualified Data.Text    as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
    let owner = "phadej"
        repo  = "github"
    result <- GH.starRepo (GH.OAuth "your-token")
        (GH.mkOwnerName owner) (GH.mkRepoName repo)
    case result of
        Left err ->   putStrLn $ "Error: " ++ show err
        Right () -> T.putStrLn $ T.concat ["Starred: ", owner, "/", repo]
