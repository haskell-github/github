{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Common
import Prelude ()

import Control.Monad.Operational
import Control.Monad.Trans.Except  (ExceptT (..), runExceptT)
import Network.HTTP.Client         (Manager, newManager, ManagerSettings)
import Network.HTTP.Client.OpenSSL (opensslManagerSettings, withOpenSSL)

import qualified GitHub                   as GH
import qualified OpenSSL.Session          as SSL
import qualified OpenSSL.X509.SystemStore as SSL

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
main = withOpenSSL $ do
    manager <- newManager tlsManagerSettings
    auth' <- getAuth
    case auth' of
        Nothing -> return ()
        Just auth -> do
            owner <- runExceptT $ runMonad manager auth $ do
                repo <- githubRequest $ GH.repositoryR "phadej" "github"
                githubRequest $ GH.ownerInfoForR (GH.simpleOwnerLogin . GH.repoOwner $ repo)
            print owner

tlsManagerSettings :: ManagerSettings
tlsManagerSettings = opensslManagerSettings $ do
    ctx <- SSL.context
    SSL.contextAddOption ctx SSL.SSL_OP_NO_SSLv2
    SSL.contextAddOption ctx SSL.SSL_OP_NO_SSLv3
    SSL.contextAddOption ctx SSL.SSL_OP_NO_TLSv1
    SSL.contextSetCiphers ctx "ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES256-SHA384:ECDHE-RSA-AES256-SHA384:ECDHE-ECDSA-AES128-SHA256:ECDHE-RSA-AES128-SHA256"
    SSL.contextLoadSystemCerts ctx
    SSL.contextSetVerificationMode ctx $ SSL.VerifyPeer True True Nothing
    return ctx
