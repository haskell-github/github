{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- This module re-exports the @Github.Data.@ and "Github.Auth" submodules.
module Github.Data (
    -- * Tagged types
    -- ** Name
    Name,
    mkName,
    untagName,
    mkOwnerName,
    mkTeamName,
    mkOrganizationName,
    mkRepoName,
    fromUserName,
    fromOrganizationName,
    -- ** Id
    Id,
    mkId,
    untagId,
    mkOwnerId,
    mkTeamId,
    mkOrganizationId,
    mkRepoId,
    -- * Module re-exports
    module Github.Auth,
    module Github.Data.Comments,
    module Github.Data.Content,
    module Github.Data.Definitions,
    module Github.Data.Gists,
    module Github.Data.GitData,
    module Github.Data.Issues,
    module Github.Data.PullRequests,
    module Github.Data.Repos,
    module Github.Data.Request,
    module Github.Data.Search,
    module Github.Data.Teams,
    module Github.Data.Webhooks,
    ) where

import Prelude        ()
import Prelude.Compat

import Data.Text (Text)

import Github.Auth
import Github.Data.Comments
import Github.Data.Content
import Github.Data.Definitions
import Github.Data.Gists
import Github.Data.GitData
import Github.Data.Id
import Github.Data.Issues
import Github.Data.Name
import Github.Data.PullRequests
import Github.Data.Repos
import Github.Data.Request
import Github.Data.Search
import Github.Data.Teams
import Github.Data.Webhooks

mkOwnerId :: Int -> Id GithubOwner
mkOwnerId = Id

mkOwnerName :: Text -> Name GithubOwner
mkOwnerName = N

mkTeamId :: Int -> Id Team
mkTeamId = Id

mkTeamName :: Text -> Name Team
mkTeamName = N

mkOrganizationId :: Int -> Id Organization
mkOrganizationId = Id

mkOrganizationName :: Text -> Name Organization
mkOrganizationName = N

mkRepoId :: Int -> Id Repo
mkRepoId = Id

mkRepoName :: Text -> Name Repo
mkRepoName = N

fromOrganizationName :: Name Organization -> Name GithubOwner
fromOrganizationName = N . untagName

fromUserName :: Name User -> Name GithubOwner
fromUserName = N . untagName
