{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The pull requests API as documented at
-- <http://developer.github.com/v3/pulls/>.
module Github.PullRequests (
    pullRequestsFor'',
    pullRequestsFor',
    pullRequestsFor,
    pullRequestsForR,
    pullRequest',
    pullRequest,
    pullRequestR,
    createPullRequest,
    createPullRequestR,
    updatePullRequest,
    updatePullRequestR,
    pullRequestCommits',
    pullRequestCommitsIO,
    pullRequestCommitsR,
    pullRequestFiles',
    pullRequestFiles,
    pullRequestFilesR,
    isPullRequestMerged,
    isPullRequestMergedR,
    mergePullRequest,
    mergePullRequestR,
    module Github.Data
    ) where

import Github.Auth
import Github.Data
import Github.Request

import Data.Aeson.Compat  (Value, encode, object, (.=))
import Data.Vector        (Vector)
import Network.HTTP.Types

import qualified Data.ByteString.Char8 as BS8

-- | All pull requests for the repo, by owner, repo name, and pull request state.
-- | With authentification
--
-- > pullRequestsFor' (Just ("github-username", "github-password"))  (Just "open") "rails" "rails"
--
-- State can be one of @all@, @open@, or @closed@. Default is @open@.
--
pullRequestsFor'' :: Maybe GithubAuth -> Maybe String -> Name GithubOwner -> Name Repo -> IO (Either Error (Vector SimplePullRequest))
pullRequestsFor'' auth state user repo =
    executeRequestMaybe auth $ pullRequestsForR user repo state Nothing

-- | All pull requests for the repo, by owner and repo name.
-- | With authentification
--
-- > pullRequestsFor' (Just ("github-username", "github-password")) "rails" "rails"
pullRequestsFor' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> IO (Either Error (Vector SimplePullRequest))
pullRequestsFor' auth = pullRequestsFor'' auth Nothing

-- | All pull requests for the repo, by owner and repo name.
--
-- > pullRequestsFor "rails" "rails"
pullRequestsFor :: Name GithubOwner -> Name Repo -> IO (Either Error (Vector SimplePullRequest))
pullRequestsFor = pullRequestsFor'' Nothing Nothing

-- | List pull requests.
-- See <https://developer.github.com/v3/pulls/#list-pull-requests>
pullRequestsForR :: Name GithubOwner -> Name Repo
                 -> Maybe String  -- ^ State
                 -> Maybe Count
                 -> GithubRequest k (Vector SimplePullRequest)
pullRequestsForR user repo state =
    GithubPagedGet ["repos", toPathPart user, toPathPart repo, "pulls"] qs
  where
    qs = maybe [] (\s -> [("state", Just . BS8.pack $ s)]) state

-- | A detailed pull request, which has much more information. This takes the
-- repo owner and name along with the number assigned to the pull request.
-- | With authentification
--
-- > pullRequest' (Just ("github-username", "github-password")) "thoughtbot" "paperclip" 562
pullRequest' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> Id PullRequest -> IO (Either Error PullRequest)
pullRequest' auth user repo prid =
    executeRequestMaybe auth $ pullRequestR user repo prid

-- | A detailed pull request, which has much more information. This takes the
-- repo owner and name along with the number assigned to the pull request.
--
-- > pullRequest "thoughtbot" "paperclip" 562
pullRequest :: Name GithubOwner -> Name Repo -> Id PullRequest -> IO (Either Error PullRequest)
pullRequest = pullRequest' Nothing

-- | Get a single pull request.
-- See <https://developer.github.com/v3/pulls/#get-a-single-pull-request>
pullRequestR :: Name GithubOwner -> Name Repo -> Id PullRequest -> GithubRequest k PullRequest
pullRequestR user repo prid =
    GithubGet ["repos", toPathPart user, toPathPart repo, "pulls", toPathPart prid] []

createPullRequest :: GithubAuth
                  -> Name GithubOwner
                  -> Name Repo
                  -> CreatePullRequest
                  -> IO (Either Error PullRequest)
createPullRequest auth user repo cpr =
    executeRequest auth $ createPullRequestR user repo cpr

-- | Create a pull request.
-- See <https://developer.github.com/v3/pulls/#create-a-pull-request>
createPullRequestR :: Name GithubOwner
                   -> Name Repo
                   -> CreatePullRequest
                   -> GithubRequest 'True PullRequest
createPullRequestR user repo cpr =
    GithubPost Post ["repos", toPathPart user, toPathPart repo, "pulls"] (encode cpr)

-- | Update a pull request
updatePullRequest :: GithubAuth -> Name GithubOwner -> Name Repo -> Id PullRequest -> EditPullRequest -> IO (Either Error PullRequest)
updatePullRequest auth user repo prid epr =
    executeRequest auth $ updatePullRequestR user repo prid epr

-- | Update a pull request.
-- See <https://developer.github.com/v3/pulls/#update-a-pull-request>
updatePullRequestR :: Name GithubOwner
                   -> Name Repo
                   -> Id PullRequest
                   -> EditPullRequest
                   -> GithubRequest 'True PullRequest
updatePullRequestR user repo prid epr =
    GithubPost Patch ["repos", toPathPart user, toPathPart repo, "pulls", toPathPart prid] (encode epr)

-- | All the commits on a pull request, given the repo owner, repo name, and
-- the number of the pull request.
-- | With authentification
--
-- > pullRequestCommits' (Just ("github-username", "github-password")) "thoughtbot" "paperclip" 688
pullRequestCommits' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> Id PullRequest -> IO (Either Error (Vector Commit))
pullRequestCommits' auth user repo prid =
    executeRequestMaybe auth $ pullRequestCommitsR user repo prid Nothing

-- | All the commits on a pull request, given the repo owner, repo name, and
-- the number of the pull request.
--
-- > pullRequestCommits "thoughtbot" "paperclip" 688
pullRequestCommitsIO :: Name GithubOwner -> Name Repo -> Id PullRequest -> IO (Either Error (Vector Commit))
pullRequestCommitsIO = pullRequestCommits' Nothing

-- | List commits on a pull request.
-- See <https://developer.github.com/v3/pulls/#list-commits-on-a-pull-request>
pullRequestCommitsR :: Name GithubOwner -> Name Repo -> Id PullRequest -> Maybe Count -> GithubRequest k (Vector Commit)
pullRequestCommitsR user repo prid =
    GithubPagedGet ["repos", toPathPart user, toPathPart repo, "pulls", toPathPart prid, "commits"] []

-- | The individual files that a pull request patches. Takes the repo owner and
-- name, plus the number assigned to the pull request.
-- | With authentification
--
-- > pullRequestFiles' (Just ("github-username", "github-password")) "thoughtbot" "paperclip" 688
pullRequestFiles' :: Maybe GithubAuth -> Name GithubOwner -> Name Repo -> Id PullRequest -> IO (Either Error (Vector File))
pullRequestFiles' auth user repo prid =
    executeRequestMaybe auth $ pullRequestFilesR user repo prid Nothing

-- | The individual files that a pull request patches. Takes the repo owner and
-- name, plus the number assigned to the pull request.
--
-- > pullRequestFiles "thoughtbot" "paperclip" 688
pullRequestFiles :: Name GithubOwner -> Name Repo -> Id PullRequest -> IO (Either Error (Vector File))
pullRequestFiles = pullRequestFiles' Nothing

-- | List pull requests files.
-- See <https://developer.github.com/v3/pulls/#list-pull-requests-files>
pullRequestFilesR :: Name GithubOwner -> Name Repo -> Id PullRequest -> Maybe Count -> GithubRequest k (Vector File)
pullRequestFilesR user repo prid =
    GithubPagedGet ["repos", toPathPart user, toPathPart repo, "pulls", toPathPart prid, "files"] []

-- | Check if pull request has been merged.
isPullRequestMerged :: GithubAuth -> Name GithubOwner -> Name Repo -> Id PullRequest -> IO (Either Error Status)
isPullRequestMerged auth user repo prid =
    executeRequest auth $ isPullRequestMergedR user repo prid

-- | Get if a pull request has been merged.
-- See <https://developer.github.com/v3/pulls/#get-if-a-pull-request-has-been-merged>
isPullRequestMergedR :: Name GithubOwner -> Name Repo -> Id PullRequest -> GithubRequest k Status
isPullRequestMergedR user repo prid = GithubStatus $
    GithubGet ["repos", toPathPart user, toPathPart repo, "pulls", toPathPart prid, "merge"] []

-- | Merge a pull request.
mergePullRequest :: GithubAuth -> Name GithubOwner -> Name Repo -> Id PullRequest -> Maybe String -> IO (Either Error Status)
mergePullRequest auth user repo prid commitMessage =
    executeRequest auth $ mergePullRequestR user repo prid commitMessage

-- | Merge a pull request (Merge Button).
-- https://developer.github.com/v3/pulls/#merge-a-pull-request-merge-button
mergePullRequestR :: Name GithubOwner -> Name Repo -> Id PullRequest -> Maybe String -> GithubRequest 'True Status
mergePullRequestR user repo prid commitMessage = GithubStatus $
    GithubPost Put paths (encode $ buildCommitMessageMap commitMessage)
  where
    paths = ["repos", toPathPart user, toPathPart repo, "pulls", toPathPart prid, "merge"]

    buildCommitMessageMap :: Maybe String -> Value
    buildCommitMessageMap (Just msg) = object ["commit_message" .= msg ]
    buildCommitMessageMap Nothing    = object []

