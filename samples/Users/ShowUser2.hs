{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Github.All as GH

main :: IO ()
main = do
    possibleUser <- GH.executeRequest' $ GH.userInfoForR "phadej"
    print possibleUser
