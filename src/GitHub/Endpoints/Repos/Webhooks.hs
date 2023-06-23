-- |
-- The webhooks API, as described at
-- <https://developer.github.com/v3/repos/hooks/>
-- <https://developer.github.com/webhooks>

module GitHub.Endpoints.Repos.Webhooks (
    -- * Querying repositories
    webhooksForR,
    webhookForR,

    -- ** Create
    createRepoWebhookR,

    -- ** Edit
    editRepoWebhookR,

    -- ** Test
    testPushRepoWebhookR,
    pingRepoWebhookR,

    -- ** Delete
    deleteRepoWebhookR,
) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

-- | List hooks.
-- See <https://developer.github.com/v3/repos/hooks/#list-hooks>
webhooksForR :: Name Owner -> Name Repo -> FetchCount -> Request k (Vector RepoWebhook)
webhooksForR user repo =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "hooks"] []
-- See <https://developer.github.com/v3/repos/hooks/#get-single-hook>
webhookForR :: Name Owner -> Name Repo -> Id RepoWebhook -> Request k RepoWebhook
webhookForR user repo hookId =
    query ["repos", toPathPart user, toPathPart repo, "hooks", toPathPart hookId] []

-- | Create a hook.
-- See <https://developer.github.com/v3/repos/hooks/#create-a-hook>
createRepoWebhookR :: Name Owner -> Name Repo -> NewRepoWebhook -> Request 'RW RepoWebhook
createRepoWebhookR user repo hook =
    command Post ["repos", toPathPart user, toPathPart repo, "hooks"] (encode hook)

-- | Edit a hook.
-- See <https://developer.github.com/v3/repos/hooks/#edit-a-hook>
editRepoWebhookR :: Name Owner -> Name Repo -> Id RepoWebhook -> EditRepoWebhook -> Request 'RW RepoWebhook
editRepoWebhookR user repo hookId hookEdit =
    command Patch ["repos", toPathPart user, toPathPart repo, "hooks", toPathPart hookId] (encode hookEdit)

-- | Test a push hook.
-- See <https://developer.github.com/v3/repos/hooks/#test-a-push-hook>
testPushRepoWebhookR :: Name Owner -> Name Repo -> Id RepoWebhook -> GenRequest 'MtStatus 'RW Bool
testPushRepoWebhookR user repo hookId =
    Command Post (createWebhookOpPath user repo hookId $ Just "tests") (encode ())

-- | Ping a hook.
-- See <https://developer.github.com/v3/repos/hooks/#ping-a-hook>
pingRepoWebhookR :: Name Owner -> Name Repo -> Id RepoWebhook -> GenRequest 'MtStatus 'RW Bool
pingRepoWebhookR user repo hookId =
    Command Post (createWebhookOpPath user repo hookId $ Just "pings") (encode ())

-- | Delete a hook.
-- See <https://developer.github.com/v3/repos/hooks/#delete-a-hook>
deleteRepoWebhookR :: Name Owner -> Name Repo -> Id RepoWebhook -> GenRequest 'MtUnit 'RW ()
deleteRepoWebhookR user repo hookId =
    Command Delete (createWebhookOpPath user repo hookId Nothing) mempty

createBaseWebhookPath :: Name Owner -> Name Repo -> Id RepoWebhook -> Paths
createBaseWebhookPath user repo hookId =
    ["repos", toPathPart user, toPathPart repo, "hooks", toPathPart hookId]

createWebhookOpPath :: Name Owner -> Name Repo -> Id RepoWebhook -> Maybe Text -> Paths
createWebhookOpPath owner reqName webhookId Nothing = createBaseWebhookPath owner reqName webhookId
createWebhookOpPath owner reqName webhookId (Just operation) = createBaseWebhookPath owner reqName webhookId ++ [operation]
