{-# LANGUAGE DataKinds #-}
-- | The webhooks API, as described at
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

import Github.Auth
import Github.Data
import Github.Request

import Data.Aeson.Compat  (encode)
import Network.HTTP.Types (Status)

webhooksFor' :: GithubAuth -> Name GithubOwner -> Name Repo -> IO (Either Error [RepoWebhook])
webhooksFor' auth user repo =
    executeRequest auth $ webhooksForR user repo

-- | List hooks.
-- See <https://developer.github.com/v3/repos/hooks/#list-hooks>
webhooksForR :: Name GithubOwner -> Name Repo -> GithubRequest k [RepoWebhook]
webhooksForR user repo =
    GithubGet ["repos", untagName user, untagName repo, "hooks"] []

webhookFor' :: GithubAuth -> Name GithubOwner -> Name Repo -> Id RepoWebhook -> IO (Either Error RepoWebhook)
webhookFor' auth user repo hookId =
    executeRequest auth $ webhookForR user repo hookId

-- | Get single hook.
-- See <https://developer.github.com/v3/repos/hooks/#get-single-hook>
webhookForR :: Name GithubOwner -> Name Repo -> Id RepoWebhook -> GithubRequest k RepoWebhook
webhookForR user repo hookId =
    GithubGet ["repos", untagName user, untagName repo, "hooks", show $ untagId hookId] []

createRepoWebhook' :: GithubAuth -> Name GithubOwner -> Name Repo -> NewRepoWebhook -> IO (Either Error RepoWebhook)
createRepoWebhook' auth user repo hook =
    executeRequest auth $ createRepoWebhookR user repo hook

-- | Create a hook.
-- See <https://developer.github.com/v3/repos/hooks/#create-a-hook>
createRepoWebhookR :: Name GithubOwner -> Name Repo -> NewRepoWebhook -> GithubRequest 'True RepoWebhook
createRepoWebhookR user repo hook =
    GithubPost Post ["repos", untagName user, untagName repo, "hooks"] (encode hook)

editRepoWebhook' :: GithubAuth -> Name GithubOwner -> Name Repo -> Id RepoWebhook -> EditRepoWebhook -> IO (Either Error RepoWebhook)
editRepoWebhook' auth user repo hookId hookEdit =
    executeRequest auth $ editRepoWebhookR user repo hookId hookEdit

-- | Edit a hook.
-- See <https://developer.github.com/v3/repos/hooks/#edit-a-hook>
editRepoWebhookR :: Name GithubOwner -> Name Repo -> Id RepoWebhook -> EditRepoWebhook -> GithubRequest 'True RepoWebhook
editRepoWebhookR user repo hookId hookEdit =
    GithubPost Patch ["repos", untagName user, untagName repo, "hooks", show $ untagId hookId] (encode hookEdit)

testPushRepoWebhook' :: GithubAuth -> Name GithubOwner -> Name Repo -> Id RepoWebhook -> IO (Either Error Status)
testPushRepoWebhook' auth user repo hookId =
    executeRequest auth $ testPushRepoWebhookR user repo hookId

-- | Test a push hook.
-- See <https://developer.github.com/v3/repos/hooks/#test-a-push-hook>
testPushRepoWebhookR :: Name GithubOwner -> Name Repo -> Id RepoWebhook -> GithubRequest 'True Status
testPushRepoWebhookR user repo hookId = GithubStatus $
    GithubPost Post (createWebhookOpPath user repo hookId $ Just "tests") (encode ())

pingRepoWebhook' :: GithubAuth -> Name GithubOwner -> Name Repo -> Id RepoWebhook -> IO (Either Error Status)
pingRepoWebhook' auth user repo hookId =
    executeRequest auth $ pingRepoWebhookR user repo hookId

-- | Ping a hook.
-- See <https://developer.github.com/v3/repos/hooks/#ping-a-hook>
pingRepoWebhookR :: Name GithubOwner -> Name Repo -> Id RepoWebhook -> GithubRequest 'True Status
pingRepoWebhookR user repo hookId = GithubStatus $
    GithubPost Post (createWebhookOpPath user repo hookId $ Just "pings") (encode ())

deleteRepoWebhook' :: GithubAuth -> Name GithubOwner -> Name Repo -> Id RepoWebhook -> IO (Either Error ())
deleteRepoWebhook' auth user repo hookId =
    executeRequest auth $ deleteRepoWebhookR user repo hookId

-- | Delete a hook.
-- See <https://developer.github.com/v3/repos/hooks/#delete-a-hook>
deleteRepoWebhookR :: Name GithubOwner -> Name Repo -> Id RepoWebhook -> GithubRequest 'True ()
deleteRepoWebhookR user repo hookId =
    GithubDelete $ createWebhookOpPath user repo hookId Nothing

createBaseWebhookPath :: Name GithubOwner -> Name Repo -> Id RepoWebhook -> [String]
createBaseWebhookPath user repo hookId =
    ["repos", untagName user, untagName repo, "hooks", show $ untagId hookId]

createWebhookOpPath :: Name GithubOwner -> Name Repo -> Id RepoWebhook -> Maybe String -> [String]
createWebhookOpPath owner reqName webhookId Nothing = createBaseWebhookPath owner reqName webhookId
createWebhookOpPath owner reqName webhookId (Just operation) = createBaseWebhookPath owner reqName webhookId ++ [operation]
