module CreateWebhook where

import Github.Repos.Webhooks
import qualified Github.Auth as Auth
import Github.Data.Definitions
import qualified Data.Map as M

main :: IO ()
main = do
  let auth = Auth.GithubOAuth "oauthtoken"
  let config = M.fromList [("url", "https://foo3.io"), ("content_type", "application/json"), ("insecure_ssl", "1")]
  let webhookDef = NewRepoWebhook {
        newRepoWebhookName = "web",
        newRepoWebhookConfig = config,
        newRepoWebhookEvents = Just [WebhookWildcardEvent],
        newRepoWebhookActive = Just True
      }
  newWebhook <- createRepoWebhook' auth "repoOwner" "repoName" webhookDef
  case newWebhook of
    (Left err) -> putStrLn $ "Error: " ++ (show err)
    (Right webhook) -> putStrLn $ formatRepoWebhook webhook

formatRepoWebhook :: RepoWebhook -> String
formatRepoWebhook (RepoWebhook _ _ _ name _ _ _ _ _ _) = show name
