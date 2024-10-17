{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Common
import Prelude ()

import Control.Exception          (throw)
import Control.Monad.IO.Class     (liftIO)
import Control.Monad.Operational  (Program, ProgramViewT (..), singleton, view)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Network.HTTP.Client        (Manager, newManager, responseBody)

import qualified GitHub as GH

data R a where
    R :: FromJSON a => GH.Request 'GH.RA a -> R a

type GithubMonad a = Program R a

runMonad :: GH.AuthMethod auth => Manager -> auth -> GithubMonad a -> ExceptT GH.Error IO a
runMonad mgr auth m = case view m of
    Return a   -> return a
    R req :>>= k -> do
        res <- ExceptT $ GH.executeRequestWithMgrAndRes mgr auth req
        liftIO $ print $ GH.limitsFromHttpResponse res
        runMonad mgr auth (k (responseBody res))

githubRequest :: FromJSON a => GH.Request 'GH.RA a -> GithubMonad a
githubRequest = singleton . R

main :: IO ()
main = GH.withOpenSSL $ do
    manager <- newManager GH.tlsManagerSettings
    auth' <- getAuth
    case auth' of
        Nothing -> do
            (owner, rl) <- runExceptT (runMonad manager () script) >>= either throw return
            print owner
            print rl
        Just auth -> do
            (owner, rl) <- runExceptT (runMonad manager auth script) >>= either throw return
            print owner
            print rl

script :: Program R (GH.Owner, GH.Limits)
script = do
    repo <- githubRequest $ GH.repositoryR "haskell-github" "github"
    owner <- githubRequest $ GH.ownerInfoForR (GH.simpleOwnerLogin . GH.repoOwner $ repo)
    rl <- githubRequest GH.rateLimitR
    return (owner, GH.rateLimitCore rl)
