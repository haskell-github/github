{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified GitHub as GH

main :: IO ()
main = do
    possibleUser <- GH.executeRequest' $ GH.userInfoForR "phadej"
    print possibleUser
