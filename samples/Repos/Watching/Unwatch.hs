{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified GitHub as GH
import qualified Data.Text    as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
    let auth = GH.BasicAuth "<login>" "<pass>"
        owner = "haskell-github"
        repo  = "github"
    result <- GH.github auth GH.unwatchRepoR (GH.mkOwnerName owner) (GH.mkRepoName repo)
    case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right () -> T.putStrLn $ T.concat ["No longer watching: ", owner, "/", repo]
