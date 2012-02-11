-- | The organization members API as described on
-- <http://developer.github.com/v3/orgs/members/>.
module Github.Organizations.Members (
 membersOf
,module Github.Data
) where

import Github.Data
import Github.Private

-- | All the users who are members of the specified organization.
--
-- > membersOf def "thoughtbot"
membersOf :: GithubConfig -> String -> IO (Either Error [GithubOwner])
membersOf c organization = githubGet c ["orgs", organization, "members"]
