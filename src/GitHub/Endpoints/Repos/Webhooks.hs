 -----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The webhooks API, as described at
-- <https://developer.github.com/v3/repos/hooks/>
-- <https://developer.github.com/webhooks>
module GitHub.Endpoints.Repos.Webhooks (
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

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

webhooksFor' :: Auth -> Name Owner -> Name Repo -> IO (Either Error (Vector RepoWebhook))
webhooksFor' auth user repo =
    executeRequest auth $ webhooksForR user repo FetchAll

-- | List hooks.
-- See <https://developer.github.com/v3/repos/hooks/#list-hooks>
webhooksForR :: Name Owner -> Name Repo -> FetchCount -> Request k (Vector RepoWebhook)
webhooksForR user repo =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "hooks"] []

webhookFor' :: Auth -> Name Owner -> Name Repo -> Id RepoWebhook -> IO (Either Error RepoWebhook)
webhookFor' auth user repo hookId =
    executeRequest auth $ webhookForR user repo hookId

-- | Query single hook.
-- See <https://developer.github.com/v3/repos/hooks/#get-single-hook>
webhookForR :: Name Owner -> Name Repo -> Id RepoWebhook -> Request k RepoWebhook
webhookForR user repo hookId =
    query ["repos", toPathPart user, toPathPart repo, "hooks", toPathPart hookId] []

createRepoWebhook' :: Auth -> Name Owner -> Name Repo -> NewRepoWebhook -> IO (Either Error RepoWebhook)
createRepoWebhook' auth user repo hook =
    executeRequest auth $ createRepoWebhookR user repo hook

-- | Create a hook.
-- See <https://developer.github.com/v3/repos/hooks/#create-a-hook>
createRepoWebhookR :: Name Owner -> Name Repo -> NewRepoWebhook -> Request 'RW RepoWebhook
createRepoWebhookR user repo hook =
    command Post ["repos", toPathPart user, toPathPart repo, "hooks"] (encode hook)

editRepoWebhook' :: Auth -> Name Owner -> Name Repo -> Id RepoWebhook -> EditRepoWebhook -> IO (Either Error RepoWebhook)
editRepoWebhook' auth user repo hookId hookEdit =
    executeRequest auth $ editRepoWebhookR user repo hookId hookEdit

-- | Edit a hook.
-- See <https://developer.github.com/v3/repos/hooks/#edit-a-hook>
editRepoWebhookR :: Name Owner -> Name Repo -> Id RepoWebhook -> EditRepoWebhook -> Request 'RW RepoWebhook
editRepoWebhookR user repo hookId hookEdit =
    command Patch ["repos", toPathPart user, toPathPart repo, "hooks", toPathPart hookId] (encode hookEdit)

testPushRepoWebhook' :: Auth -> Name Owner -> Name Repo -> Id RepoWebhook -> IO (Either Error Bool)
testPushRepoWebhook' auth user repo hookId =
    executeRequest auth $ testPushRepoWebhookR user repo hookId

-- | Test a push hook.
-- See <https://developer.github.com/v3/repos/hooks/#test-a-push-hook>
testPushRepoWebhookR :: Name Owner -> Name Repo -> Id RepoWebhook -> GenRequest 'MtStatus 'RW Bool
testPushRepoWebhookR user repo hookId =
    Command Post (createWebhookOpPath user repo hookId $ Just "tests") (encode ())

pingRepoWebhook' :: Auth -> Name Owner -> Name Repo -> Id RepoWebhook -> IO (Either Error Bool)
pingRepoWebhook' auth user repo hookId =
    executeRequest auth $ pingRepoWebhookR user repo hookId

-- | Ping a hook.
-- See <https://developer.github.com/v3/repos/hooks/#ping-a-hook>
pingRepoWebhookR :: Name Owner -> Name Repo -> Id RepoWebhook -> GenRequest 'MtStatus 'RW Bool
pingRepoWebhookR user repo hookId =
    Command Post (createWebhookOpPath user repo hookId $ Just "pings") (encode ())

deleteRepoWebhook' :: Auth -> Name Owner -> Name Repo -> Id RepoWebhook -> IO (Either Error ())
deleteRepoWebhook' auth user repo hookId =
    executeRequest auth $ deleteRepoWebhookR user repo hookId

-- | Delete a hook.
-- See <https://developer.github.com/v3/repos/hooks/#delete-a-hook>
deleteRepoWebhookR :: Name Owner -> Name Repo -> Id RepoWebhook -> Request 'RW ()
deleteRepoWebhookR user repo hookId =
    command Delete (createWebhookOpPath user repo hookId Nothing) mempty

createBaseWebhookPath :: Name Owner -> Name Repo -> Id RepoWebhook -> Paths
createBaseWebhookPath user repo hookId =
    ["repos", toPathPart user, toPathPart repo, "hooks", toPathPart hookId]

createWebhookOpPath :: Name Owner -> Name Repo -> Id RepoWebhook -> Maybe Text -> Paths
createWebhookOpPath owner reqName webhookId Nothing = createBaseWebhookPath owner reqName webhookId
createWebhookOpPath owner reqName webhookId (Just operation) = createBaseWebhookPath owner reqName webhookId ++ [operation]
