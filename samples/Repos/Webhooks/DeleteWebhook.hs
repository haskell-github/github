module DeleteWebhook where

import Github.Repos.Webhooks
import qualified Github.Auth as Auth

main :: IO ()
main = do
  let auth = Auth.OAuth "oauthtoken"
  resp <- deleteRepoWebhook' auth "repoOwner" "repoName" 123
  case resp of
    (Left err) -> putStrLn $ "Error: " ++ (show err)
    (Right stat) -> putStrLn $ "Resp: " ++ (show stat)
