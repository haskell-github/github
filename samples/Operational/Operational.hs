{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Common
import Prelude ()

import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Operational
import Network.HTTP.Client        (Manager, newManager)
import Network.HTTP.Client.TLS    (tlsManagerSettings)

import qualified GitHub as GH

type GithubMonad a = Program (GH.Request 'GH.RA) a

runMonad :: Manager -> GH.Auth -> GithubMonad a -> ExceptT GH.Error IO a
runMonad mgr auth m = case view m of
    Return a   -> return a
    req :>>= k -> do
        b <- ExceptT $ GH.executeRequestWithMgr mgr auth req
        runMonad mgr auth (k b)

githubRequest :: GH.Request 'GH.RA a -> GithubMonad a
githubRequest = singleton

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
