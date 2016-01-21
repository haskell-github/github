module EditWebhook where

import Github.Repos.Webhooks
import qualified Github.Auth as Auth
import Github.Data.Definitions

main :: IO ()
main = do
  let auth = Auth.OAuth "oauthtoken"
  let editWebhookDef = EditRepoWebhook {
        editRepoWebhookRemoveEvents = Just [WebhookWildcardEvent],
        editRepoWebhookAddEvents = Just [WebhookCommitCommentEvent, WebhookGollumEvent],
        editRepoWebhookConfig = Nothing,
        editRepoWebhookEvents = Nothing,
        editRepoWebhookActive = Just True
      }
  newWebhook <- editRepoWebhook' auth "repoOwner" "repoName" 123 editWebhookDef
  case newWebhook of
    (Left err) -> putStrLn $ "Error: " ++ (show err)
    (Right webhook) -> putStrLn $ formatRepoWebhook webhook

formatRepoWebhook :: RepoWebhook -> String
formatRepoWebhook (RepoWebhook _ _ _ name _ _ _ _ _ _) = show name
