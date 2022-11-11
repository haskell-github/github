-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The actions API as documented at
-- <https://docs.github.com/en/rest/reference/actions>.
module GitHub.Endpoints.Actions.Secrets (
    organizationSecretsR,
    organizationPublicKeyR,
    organizationSecretR,
    setOrganizationSecretR,
    deleteOrganizationSecretR,
    organizationSelectedRepositoriesForSecretR,
    setOrganizationSelectedRepositoriesForSecretR,
    addOrganizationSelectedRepositoriesForSecretR,
    removeOrganizationSelectedRepositoriesForSecretR,
    repoSecretsR,
    repoPublicKeyR,
    repoSecretR,
    setRepoSecretR,
    deleteRepoSecretR,
    environmentSecretsR,
    environmentPublicKeyR,
    environmentSecretR,
    setEnvironmentSecretR,
    deleteEnvironmentSecretR,
    module GitHub.Data
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

-- | List organization secrets.
-- See <https://docs.github.com/en/rest/actions/secrets#list-organization-secrets>
organizationSecretsR :: Name Organization -> GenRequest 'MtJSON 'RA (WithTotalCount Secret)
organizationSecretsR org =
    Query ["orgs", toPathPart org, "actions", "secrets"] []

-- | List organization secrets.
-- See <https://docs.github.com/en/rest/actions/secrets#get-an-organization-public-key>
organizationPublicKeyR :: Name Organization -> GenRequest 'MtJSON 'RA PublicKey
organizationPublicKeyR org =
    Query ["orgs", toPathPart org, "actions", "secrets", "public-key"] []

-- | Get an organization secret.
-- See <https://docs.github.com/en/rest/actions/secrets#get-an-organization-secret>
organizationSecretR :: Name Organization -> Name Secret -> GenRequest 'MtJSON 'RA Secret
organizationSecretR org name =
    Query ["orgs", toPathPart org, "actions", "secrets", toPathPart name] []

-- | Create or update an organization secret.
-- See <https://docs.github.com/en/rest/actions/secrets#create-or-update-an-organization-secret>
setOrganizationSecretR :: Name Organization -> Name Secret -> SetSecret -> GenRequest 'MtJSON 'RW ()
setOrganizationSecretR org name =
    command Put ["orgs", toPathPart org, "actions", "secrets", toPathPart name] . encode

-- | Delete an organization secret.
-- See <https://docs.github.com/en/rest/actions/secrets#delete-an-organization-secret>
deleteOrganizationSecretR :: Name Organization -> Name Secret -> Request 'RW ()
deleteOrganizationSecretR org name =
    command Delete parts mempty
    where
        parts = ["orgs", toPathPart org, "actions", "secrets", toPathPart name]

-- | Get selected repositories for an organization secret.
-- See <https://docs.github.com/en/rest/actions/secrets#list-selected-repositories-for-an-organization-secret>
organizationSelectedRepositoriesForSecretR :: Name Organization -> Name Secret -> GenRequest 'MtJSON 'RA (WithTotalCount SelectedRepo)
organizationSelectedRepositoriesForSecretR org name =
    Query ["orgs", toPathPart org, "actions", "secrets", toPathPart name, "repositories"] []

-- | Set selected repositories for an organization secret.
-- See <https://docs.github.com/en/rest/actions/secrets#set-selected-repositories-for-an-organization-secret>
setOrganizationSelectedRepositoriesForSecretR :: Name Organization -> Name Secret -> SetSelectedRepositories -> GenRequest 'MtJSON 'RW ()
setOrganizationSelectedRepositoriesForSecretR org name =
    command Put ["orgs", toPathPart org, "actions", "secrets", toPathPart name, "repositories"]  . encode

-- | Add selected repository to an organization secret.
-- See <https://docs.github.com/en/rest/actions/secrets#add-selected-repository-to-an-organization-secret>
addOrganizationSelectedRepositoriesForSecretR :: Name Organization -> Name Secret -> Id Repo -> GenRequest 'MtJSON 'RW ()
addOrganizationSelectedRepositoriesForSecretR org name repo =
    command Put ["orgs", toPathPart org, "actions", "secrets", toPathPart name, "repositories", toPathPart repo] mempty

-- | Remove selected repository from an organization secret.
-- See <https://docs.github.com/en/rest/actions/secrets#remove-selected-repository-from-an-organization-secret>
removeOrganizationSelectedRepositoriesForSecretR :: Name Organization -> Name Secret -> Id Repo -> GenRequest 'MtJSON 'RW ()
removeOrganizationSelectedRepositoriesForSecretR org name repo =
    command Delete ["orgs", toPathPart org, "actions", "secrets", toPathPart name, "repositories", toPathPart repo] mempty

-- | List repository secrets.
-- See <https://docs.github.com/en/rest/actions/secrets#list-repository-secrets>
repoSecretsR :: Name Owner -> Name Repo -> FetchCount -> GenRequest 'MtJSON 'RA (WithTotalCount RepoSecret)
repoSecretsR user repo =
    PagedQuery ["repos", toPathPart user,  toPathPart repo, "actions", "secrets"] []

-- | Get a repository public key.
-- See <https://docs.github.com/en/rest/actions/secrets#get-a-repository-public-key>
repoPublicKeyR :: Name Owner -> Name Organization -> GenRequest 'MtJSON 'RA PublicKey
repoPublicKeyR user org =
    Query ["repos", toPathPart user, toPathPart org, "actions", "secrets", "public-key"] []

-- | Get a repository secret.
-- See <https://docs.github.com/en/rest/actions/secrets#get-a-repository-secret>
repoSecretR :: Name Owner -> Name Organization -> Name Secret -> GenRequest 'MtJSON 'RA RepoSecret
repoSecretR user org name =
    Query ["repos", toPathPart user, toPathPart org, "actions", "secrets", toPathPart name] []

-- | Create or update a repository secret.
-- See <https://docs.github.com/en/rest/actions/secrets#create-or-update-a-repository-secret>
setRepoSecretR :: Name Owner -> Name Organization -> Name Secret -> SetSecret -> GenRequest 'MtJSON 'RW ()
setRepoSecretR user org name =
    command Put ["repos", toPathPart user, toPathPart org, "actions", "secrets", toPathPart name] . encode

-- | Delete a repository secret.
-- See <https://docs.github.com/en/rest/actions/secrets#delete-a-repository-secret>
deleteRepoSecretR :: Name Owner -> Name Organization -> Name Secret -> Request 'RW ()
deleteRepoSecretR user org name =
    command Delete parts mempty
    where
        parts = ["repos", toPathPart user, toPathPart org, "actions", "secrets", toPathPart name]

-- | List environment secrets.
-- See <https://docs.github.com/en/rest/actions/secrets#list-environment-secrets>
environmentSecretsR :: Id Repo -> Name Environment -> FetchCount -> GenRequest 'MtJSON 'RA (WithTotalCount RepoSecret)
environmentSecretsR repo env =
    PagedQuery ["repositories", toPathPart repo, "environments", toPathPart env, "secrets"] []

-- | Get an environment public key.
-- See <https://docs.github.com/en/rest/actions/secrets#get-an-environment-public-key>
environmentPublicKeyR :: Id Repo -> Name Environment -> GenRequest 'MtJSON 'RA PublicKey
environmentPublicKeyR repo env =
    Query ["repositories", toPathPart repo, "environments", toPathPart env, "secrets", "public-key"] []

-- | Get an environment secret
-- See <https://docs.github.com/en/rest/actions/secrets#get-an-environment-secret>
environmentSecretR :: Id Repo -> Name Environment -> Name Secret -> GenRequest 'MtJSON 'RA RepoSecret
environmentSecretR repo env name =
    Query ["repositories", toPathPart repo, "environments", toPathPart env, "secrets", toPathPart name] []

-- | Create or update an environment secret.
-- See <https://docs.github.com/en/rest/actions/secrets#create-or-update-an-environment-secret>
setEnvironmentSecretR :: Id Repo -> Name Environment -> Name Secret -> SetSecret -> GenRequest 'MtJSON 'RW ()
setEnvironmentSecretR repo env name =
    command Put ["repositories", toPathPart repo, "environments", toPathPart env, "secrets", toPathPart name] . encode

-- | Delete an environment secret.
-- See <https://docs.github.com/en/rest/actions/secrets#delete-an-environment-secret>
deleteEnvironmentSecretR :: Id Repo -> Name Environment -> Name Secret -> Request 'RW ()
deleteEnvironmentSecretR repo env name =
    command Delete parts mempty
    where
        parts = ["repositories", toPathPart repo, "environments", toPathPart env, "secrets", toPathPart name]