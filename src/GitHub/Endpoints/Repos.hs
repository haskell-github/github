-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The Github Repos API, as documented at
-- <http://developer.github.com/v3/repos/>
module GitHub.Endpoints.Repos (
    -- * Querying repositories
    currentUserReposR,
    userReposR,
    organizationReposR,
    repositoryR,
    contributorsR,
    languagesForR,
    tagsForR,
    branchesForR,

    -- ** Create
    createRepoR,
    createOrganizationRepoR,
    forkExistingRepoR,

    -- ** Edit
    editRepoR,

    -- ** Delete
    deleteRepoR,

    -- * Data
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

repoPublicityQueryString :: RepoPublicity -> QueryString
repoPublicityQueryString RepoPublicityAll     = [("type", Just "all")]
repoPublicityQueryString RepoPublicityOwner   = [("type", Just "owner")]
repoPublicityQueryString RepoPublicityMember  = [("type", Just "member")]
repoPublicityQueryString RepoPublicityPublic  = [("type", Just "public")]
repoPublicityQueryString RepoPublicityPrivate = [("type", Just "private")]

-- | List your repositories.
-- See <https://docs.github.com/en/rest/reference/repos#list-repositories-for-the-authenticated-user>
currentUserReposR :: RepoPublicity -> FetchCount -> Request k (Vector Repo)
currentUserReposR publicity =
    pagedQuery  ["user", "repos"] qs
  where
    qs = repoPublicityQueryString publicity

-- | List user repositories.
-- See <https://docs.github.com/en/rest/reference/repos#list-repositories-for-a-user>
userReposR :: Name Owner -> RepoPublicity -> FetchCount -> Request k(Vector Repo)
userReposR user publicity =
    pagedQuery  ["users", toPathPart user, "repos"] qs
  where
    qs = repoPublicityQueryString publicity

-- | List organization repositories.
-- See <https://docs.github.com/en/rest/reference/repos#list-organization-repositories>
organizationReposR
    :: Name Organization
    -> RepoPublicity
    -> FetchCount
    -> Request k (Vector Repo)
organizationReposR org publicity =
    pagedQuery ["orgs", toPathPart org, "repos"] qs
  where
    qs = repoPublicityQueryString publicity

-- | Query single repository.
-- See <https://developer.github.com/v3/repos/#get>
repositoryR :: Name Owner -> Name Repo -> Request k Repo
repositoryR user repo =
    query ["repos", toPathPart user, toPathPart repo] []

-- | Create a new repository.
-- See <https://developer.github.com/v3/repos/#create>
createRepoR :: NewRepo -> Request 'RW Repo
createRepoR nrepo =
    command Post ["user", "repos"] (encode nrepo)

-- | Fork an existing repository.
-- See <https://developer.github.com/v3/repos/forks/#create-a-fork>
-- TODO: The third paramater (an optional Organisation) is not used yet.
forkExistingRepoR :: Name Owner -> Name Repo -> Maybe (Name Owner) -> Request 'RW Repo
forkExistingRepoR owner repo _morg =
    command Post ["repos", toPathPart owner, toPathPart repo, "forks" ] mempty

-- | Create a new repository for an organization.
-- See <https://developer.github.com/v3/repos/#create>
createOrganizationRepoR :: Name Organization -> NewRepo -> Request 'RW Repo
createOrganizationRepoR org nrepo =
    command Post ["orgs", toPathPart org, "repos"] (encode nrepo)

-- | Edit an existing repository.
-- See <https://developer.github.com/v3/repos/#edit>
editRepoR :: Name Owner -> Name Repo -> EditRepo -> Request 'RW Repo
editRepoR user repo body =
    command Patch ["repos", toPathPart user, toPathPart repo] (encode b)
  where
    -- if no name is given, use curent name
    b = body {editName = editName body <|> Just repo}

-- | List contributors.
-- See <https://developer.github.com/v3/repos/#list-contributors>
contributorsR
    :: Name Owner
    -> Name Repo
    -> Bool              -- ^ Include anonymous
    -> FetchCount
    -> Request k (Vector Contributor)
contributorsR user repo anon =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "contributors"] qs
  where
    qs | anon      = [("anon", Just "true")]
       | otherwise = []

-- | List languages.
-- See <https://developer.github.com/v3/repos/#list-languages>
languagesForR :: Name Owner -> Name Repo -> Request k Languages
languagesForR user repo =
    query ["repos", toPathPart user, toPathPart repo, "languages"] []

-- | List tags.
-- See <https://developer.github.com/v3/repos/#list-tags>
tagsForR :: Name Owner -> Name Repo -> FetchCount -> Request k (Vector Tag)
tagsForR user repo =
    pagedQuery  ["repos", toPathPart user, toPathPart repo, "tags"] []

-- | List branches.
-- See <https://developer.github.com/v3/repos/#list-branches>
branchesForR :: Name Owner -> Name Repo -> FetchCount -> Request k (Vector Branch)
branchesForR user repo =
    pagedQuery  ["repos", toPathPart user, toPathPart repo, "branches"] []

-- | Delete a repository,.
-- See <https://developer.github.com/v3/repos/#delete-a-repository>
deleteRepoR :: Name Owner -> Name Repo -> GenRequest 'MtUnit 'RW ()
deleteRepoR user repo =
    Command Delete ["repos", toPathPart user, toPathPart repo] mempty
