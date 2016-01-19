{-# LANGUAGE DataKinds #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The webhooks API, as described at
-- <https://developer.github.com/v3/repos/hooks/>
-- <https://developer.github.com/webhooks>

module Github.Repos.Webhooks (
    -- * Querying repositories
    webhooksFor',
    webhooksForR,
    webhookFor',
    webhookForR,

    -- ** Create
    createRepoWebhook',
    createRepoWebhookR,

    -- ** Edit
    editRepoWebhook',
    editRepoWebhookR,

    -- ** Test
    testPushRepoWebhook',
    testPushRepoWebhookR,
    pingRepoWebhook',
    pingRepoWebhookR,

    -- ** Delete
    deleteRepoWebhook',
    deleteRepoWebhookR,
) where

import Prelude        ()
import Prelude.Compat

import Data.Aeson.Compat (encode)
import Data.Vector       (Vector)

import Github.Data
import Github.Request

webhooksFor' :: GithubAuth -> Name GithubOwner -> Name Repo -> IO (Either Error (Vector RepoWebhook))
webhooksFor' auth user repo =
    executeRequest auth $ webhooksForR user repo Nothing

-- | List hooks.
-- See <https://developer.github.com/v3/repos/hooks/#list-hooks>
webhooksForR :: Name GithubOwner -> Name Repo -> Maybe Count -> GithubRequest k (Vector RepoWebhook)
webhooksForR user repo =
    GithubPagedGet ["repos", toPathPart user, toPathPart repo, "hooks"] []

webhookFor' :: GithubAuth -> Name GithubOwner -> Name Repo -> Id RepoWebhook -> IO (Either Error RepoWebhook)
webhookFor' auth user repo hookId =
    executeRequest auth $ webhookForR user repo hookId

-- | Get single hook.
-- See <https://developer.github.com/v3/repos/hooks/#get-single-hook>
webhookForR :: Name GithubOwner -> Name Repo -> Id RepoWebhook -> GithubRequest k RepoWebhook
webhookForR user repo hookId =
    GithubGet ["repos", toPathPart user, toPathPart repo, "hooks", toPathPart hookId] []

createRepoWebhook' :: GithubAuth -> Name GithubOwner -> Name Repo -> NewRepoWebhook -> IO (Either Error RepoWebhook)
createRepoWebhook' auth user repo hook =
    executeRequest auth $ createRepoWebhookR user repo hook

-- | Create a hook.
-- See <https://developer.github.com/v3/repos/hooks/#create-a-hook>
createRepoWebhookR :: Name GithubOwner -> Name Repo -> NewRepoWebhook -> GithubRequest 'True RepoWebhook
createRepoWebhookR user repo hook =
    GithubCommand Post ["repos", toPathPart user, toPathPart repo, "hooks"] (encode hook)

editRepoWebhook' :: GithubAuth -> Name GithubOwner -> Name Repo -> Id RepoWebhook -> EditRepoWebhook -> IO (Either Error RepoWebhook)
editRepoWebhook' auth user repo hookId hookEdit =
    executeRequest auth $ editRepoWebhookR user repo hookId hookEdit

-- | Edit a hook.
-- See <https://developer.github.com/v3/repos/hooks/#edit-a-hook>
editRepoWebhookR :: Name GithubOwner -> Name Repo -> Id RepoWebhook -> EditRepoWebhook -> GithubRequest 'True RepoWebhook
editRepoWebhookR user repo hookId hookEdit =
    GithubCommand Patch ["repos", toPathPart user, toPathPart repo, "hooks", toPathPart hookId] (encode hookEdit)

testPushRepoWebhook' :: GithubAuth -> Name GithubOwner -> Name Repo -> Id RepoWebhook -> IO (Either Error Bool)
testPushRepoWebhook' auth user repo hookId =
    executeRequest auth $ testPushRepoWebhookR user repo hookId

-- | Test a push hook.
-- See <https://developer.github.com/v3/repos/hooks/#test-a-push-hook>
testPushRepoWebhookR :: Name GithubOwner -> Name Repo -> Id RepoWebhook -> GithubRequest 'True Bool
testPushRepoWebhookR user repo hookId = GithubStatus StatusOnlyOk $
    GithubCommand Post (createWebhookOpPath user repo hookId $ Just "tests") (encode ())

pingRepoWebhook' :: GithubAuth -> Name GithubOwner -> Name Repo -> Id RepoWebhook -> IO (Either Error Bool)
pingRepoWebhook' auth user repo hookId =
    executeRequest auth $ pingRepoWebhookR user repo hookId

-- | Ping a hook.
-- See <https://developer.github.com/v3/repos/hooks/#ping-a-hook>
pingRepoWebhookR :: Name GithubOwner -> Name Repo -> Id RepoWebhook -> GithubRequest 'True Bool
pingRepoWebhookR user repo hookId = GithubStatus StatusOnlyOk $
    GithubCommand Post (createWebhookOpPath user repo hookId $ Just "pings") (encode ())

deleteRepoWebhook' :: GithubAuth -> Name GithubOwner -> Name Repo -> Id RepoWebhook -> IO (Either Error ())
deleteRepoWebhook' auth user repo hookId =
    executeRequest auth $ deleteRepoWebhookR user repo hookId

-- | Delete a hook.
-- See <https://developer.github.com/v3/repos/hooks/#delete-a-hook>
deleteRepoWebhookR :: Name GithubOwner -> Name Repo -> Id RepoWebhook -> GithubRequest 'True ()
deleteRepoWebhookR user repo hookId =
    GithubCommand Delete (createWebhookOpPath user repo hookId Nothing) mempty

createBaseWebhookPath :: Name GithubOwner -> Name Repo -> Id RepoWebhook -> [String]
createBaseWebhookPath user repo hookId =
    ["repos", toPathPart user, toPathPart repo, "hooks", toPathPart hookId]

createWebhookOpPath :: Name GithubOwner -> Name Repo -> Id RepoWebhook -> Maybe String -> [String]
createWebhookOpPath owner reqName webhookId Nothing = createBaseWebhookPath owner reqName webhookId
createWebhookOpPath owner reqName webhookId (Just operation) = createBaseWebhookPath owner reqName webhookId ++ [operation]
