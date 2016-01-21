module ListWebhook where

import qualified Github.Repos.Webhooks as W
import qualified Github.Auth as Auth
import qualified Github.Data.Definitions as Def

main :: IO ()
main = do
  let auth = Auth.OAuth "oauthtoken"
  possibleWebhook <- W.webhookFor' auth "repoOwner" "repoName" 123
  case possibleWebhook of
    (Left err) -> putStrLn $ "Error: " ++ (show err)
    (Right webhook) -> putStrLn $ formatRepoWebhook webhook

formatRepoWebhook :: Def.RepoWebhook -> String
formatRepoWebhook (Def.RepoWebhook _ _ _ name _ _ _ _ _ _) = show name
