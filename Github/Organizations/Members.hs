module Github.Organizations.Members (
 membersOf
,module Github.Data
) where

import Github.Data
import Github.Private

membersOf :: String -> IO (Either Error [GithubUser])
membersOf organization = githubGet ["orgs", organization, "members"]
