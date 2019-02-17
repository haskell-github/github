-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The organization members API as described on
-- <http://developer.github.com/v3/orgs/members/>.
module GitHub.Endpoints.Organizations.Members (
    membersOf,
    membersOf',
    membersOfR,
    membersOfWithR,
    isMemberOf,
    isMemberOf',
    isMemberOfR,
    orgInvitationsR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

-- | All the users who are members of the specified organization,
-- | with or without authentication.
--
-- > membersOf' (Just $ OAuth "token") "thoughtbot"
membersOf' :: Maybe Auth -> Name Organization -> IO (Either Error (Vector SimpleUser))
membersOf' auth org =
    executeRequestMaybe auth $ membersOfR org FetchAll

-- | All the users who are members of the specified organization,
-- | without authentication.
--
-- > membersOf "thoughtbot"
membersOf :: Name Organization -> IO (Either Error (Vector SimpleUser))
membersOf = membersOf' Nothing

-- | All the users who are members of the specified organization.
--
-- See <https://developer.github.com/v3/orgs/members/#members-list>
membersOfR :: Name Organization -> FetchCount -> Request k (Vector SimpleUser)
membersOfR organization =
    pagedQuery ["orgs", toPathPart organization, "members"] []

-- | 'membersOfR' with filters.
--
-- See <https://developer.github.com/v3/orgs/members/#members-list>
membersOfWithR :: Name Organization -> OrgMemberFilter -> OrgMemberRole -> FetchCount -> Request k (Vector SimpleUser)
membersOfWithR org f r =
    pagedQuery ["orgs", toPathPart org, "members"] [("filter", Just f'), ("role", Just r')]
  where
    f' = case f of
        OrgMemberFilter2faDisabled -> "2fa_disabled"
        OrgMemberFilterAll         -> "all"
    r' = case r of
        OrgMemberRoleAll    -> "all"
        OrgMemberRoleAdmin  -> "admin"
        OrgMemberRoleMember -> "member"

-- | Check if a user is a member of an organization,
-- | with or without authentication.
--
-- > isMemberOf' (Just $ OAuth "token") "phadej" "haskell-infra"
isMemberOf' :: Maybe Auth -> Name User -> Name Organization -> IO (Either Error Bool)
isMemberOf' auth user org =
    executeRequestMaybe auth $ isMemberOfR user org

-- | Check if a user is a member of an organization,
-- | without authentication.
--
-- > isMemberOf "phadej" "haskell-infra"
isMemberOf :: Name User -> Name Organization -> IO (Either Error Bool)
isMemberOf = isMemberOf' Nothing

-- | Check if a user is a member of an organization.
--
-- See <https://developer.github.com/v3/orgs/members/#check-membership>
isMemberOfR :: Name User -> Name Organization -> GenRequest 'MtStatus rw Bool
isMemberOfR user org =
    Query [ "orgs", toPathPart org, "members", toPathPart user ] []

-- | List pending organization invitations
--
-- See <https://developer.github.com/v3/orgs/members/#list-pending-organization-invitations>
orgInvitationsR :: Name Organization -> FetchCount -> Request 'RA (Vector Invitation)
orgInvitationsR org = pagedQuery ["orgs", toPathPart org, "invitations"] []
