module ListWebhooks where

import qualified Github.Repos.Webhooks as W
import qualified Github.Auth as Auth
import qualified Github.Data.Definitions as Def
import Data.List

main :: IO ()
main = do
  let auth = Auth.OAuth "oauthtoken"
  possibleWebhooks <- W.webhooksFor' auth "repoOwner" "repoName"
  case possibleWebhooks of
    (Left err) -> putStrLn $ "Error: " ++ (show err)
    (Right webhooks) -> putStrLn $ intercalate "\n" $ map formatRepoWebhook webhooks

formatRepoWebhook :: Def.RepoWebhook -> String
formatRepoWebhook (Def.RepoWebhook _ _ _ name _ _ _ _ _ _) = show name
