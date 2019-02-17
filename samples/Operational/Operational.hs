{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Common
import Prelude ()

import Control.Monad.Operational
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Network.HTTP.Client        (Manager, newManager)
import Network.HTTP.Client.TLS    (tlsManagerSettings)

import qualified GitHub as GH

data R a where
    R :: FromJSON a => GH.Request 'GH.RA a -> R a

type GithubMonad a = Program R a

runMonad :: Manager -> GH.Auth -> GithubMonad a -> ExceptT GH.Error IO a
runMonad mgr auth m = case view m of
    Return a   -> return a
    R req :>>= k -> do
        b <- ExceptT $ GH.executeRequestWithMgr mgr auth req
        runMonad mgr auth (k b)

githubRequest :: FromJSON a => GH.Request 'GH.RA a -> GithubMonad a
githubRequest = singleton . R

main :: IO ()
main = do
    manager <- newManager tlsManagerSettings
    auth' <- getAuth
    case auth' of
        Nothing -> return ()
        Just auth -> do
            owner <- runExceptT $ runMonad manager auth $ do
                repo <- githubRequest $ GH.repositoryR "phadej" "github"
                githubRequest $ GH.ownerInfoForR (GH.simpleOwnerLogin . GH.repoOwner $ repo)
            print owner
