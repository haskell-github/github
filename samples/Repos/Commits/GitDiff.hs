{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified GitHub.Endpoints.Repos.Commits as Github
import qualified Data.Text.IO as Text

main :: IO ()
main = do
  possibleDiff <- Github.diff "thoughtbot" "paperclip" "41f685f6e01396936bb8cd98e7cca517e2c7d96b" "HEAD"
  either (fail . show) (Text.putStrLn . showDiff) possibleDiff

  -- Check special case: when a file only changes file permissions in the commits, GitHub returns a null "sha" field for that file.
  -- See https://github.com/scott-fleischman/repo-change-file-permission
  diffFillNullSha <- Github.diff "scott-fleischman" "repo-change-file-permission" "80fdf8f83fcd8181411919fbf47394b878c591a0" "77a95bbebeb78f4fb25c6a10c3c940b6fe1caa27"
  either (fail . show) (const $ Text.putStrLn "Successfully parsed diff with a file with a null sha") diffFillNullSha

  where
  showDiff diff =
    foldl (\x y -> x <> "\n\n" <> y) "" $ concatMap (maybe [] pure . Github.filePatch) $ Github.diffFiles diff
