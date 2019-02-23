-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The pull requests API as documented at
-- <http://developer.github.com/v3/pulls/>.
module GitHub.Endpoints.PullRequests (
    pullRequestsFor,
    pullRequestsFor',
    pullRequestsForR,
    pullRequest',
    pullRequest,
    pullRequestR,
    pullRequestDiff',
    pullRequestDiff,
    pullRequestDiffR,
    pullRequestPatch',
    pullRequestPatch,
    pullRequestPatchR,
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
    module GitHub.Data
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()
import Data.ByteString.Lazy (ByteString)

-- | All open pull requests for the repo, by owner and repo name.
--
-- > pullRequestsFor "rails" "rails"
pullRequestsFor :: Name Owner -> Name Repo -> IO (Either Error (Vector SimplePullRequest))
pullRequestsFor user repo =
    executeRequest' $ pullRequestsForR user repo mempty FetchAll

-- | All open pull requests for the repo, by owner and repo name.
--
-- > pullRequestsFor "rails" "rails"
pullRequestsFor' :: Maybe Auth -> Name Owner -> Name Repo -> IO (Either Error (Vector SimplePullRequest))
pullRequestsFor' auth user repo =
    executeRequestMaybe auth $ pullRequestsForR user repo mempty FetchAll

-- | List pull requests.
-- See <https://developer.github.com/v3/pulls/#list-pull-requests>
pullRequestsForR
    :: Name Owner
    -> Name Repo
    -> PullRequestMod
    -> FetchCount
    -> Request k (Vector SimplePullRequest)
pullRequestsForR user repo opts = pagedQuery
    ["repos", toPathPart user, toPathPart repo, "pulls"]
    (prModToQueryString opts)

-- | Obtain the diff of a pull request
-- See <https://developer.github.com/v3/pulls/#get-a-single-pull-request>
pullRequestDiff' :: Maybe Auth -> Name Owner -> Name Repo -> IssueNumber -> IO (Either Error ByteString)
pullRequestDiff' auth user repo prid =
    executeRequestMaybe auth $ pullRequestDiffR user repo prid

-- | Obtain the diff of a pull request
-- See <https://developer.github.com/v3/pulls/#get-a-single-pull-request>
pullRequestDiff :: Name Owner -> Name Repo -> IssueNumber -> IO (Either Error ByteString)
pullRequestDiff = pullRequestDiff' Nothing

-- | Query a single pull request to obtain the diff
-- See <https://developer.github.com/v3/pulls/#get-a-single-pull-request>
pullRequestDiffR :: Name Owner -> Name Repo -> IssueNumber -> GenRequest 'MtDiff rw ByteString
pullRequestDiffR user repo prid =
    Query ["repos", toPathPart user, toPathPart repo, "pulls", toPathPart prid] []

-- | Obtain the patch of a pull request
--
-- See <https://developer.github.com/v3/pulls/#get-a-single-pull-request>
pullRequestPatch' :: Maybe Auth -> Name Owner -> Name Repo -> IssueNumber -> IO (Either Error ByteString)
pullRequestPatch' auth user repo prid =
    executeRequestMaybe auth $ pullRequestPatchR user repo prid

-- | Obtain the patch of a pull request
-- See <https://developer.github.com/v3/pulls/#get-a-single-pull-request>
pullRequestPatch :: Name Owner -> Name Repo -> IssueNumber -> IO (Either Error ByteString)
pullRequestPatch = pullRequestPatch' Nothing

-- | Query a single pull request to obtain the patch
-- See <https://developer.github.com/v3/pulls/#get-a-single-pull-request>
pullRequestPatchR :: Name Owner -> Name Repo -> IssueNumber -> GenRequest 'MtPatch rw ByteString
pullRequestPatchR user repo prid =
    Query ["repos", toPathPart user, toPathPart repo, "pulls", toPathPart prid] []

-- | A detailed pull request, which has much more information. This takes the
-- repo owner and name along with the number assigned to the pull request.
-- With authentification.
--
-- > pullRequest' (Just ("github-username", "github-password")) "thoughtbot" "paperclip" 562
pullRequest' :: Maybe Auth -> Name Owner -> Name Repo -> IssueNumber -> IO (Either Error PullRequest)
pullRequest' auth user repo prid =
    executeRequestMaybe auth $ pullRequestR user repo prid

-- | A detailed pull request, which has much more information. This takes the
-- repo owner and name along with the number assigned to the pull request.
--
-- > pullRequest "thoughtbot" "paperclip" 562
pullRequest :: Name Owner -> Name Repo -> IssueNumber -> IO (Either Error PullRequest)
pullRequest = pullRequest' Nothing

-- | Query a single pull request.
-- See <https://developer.github.com/v3/pulls/#get-a-single-pull-request>
pullRequestR :: Name Owner -> Name Repo -> IssueNumber -> Request k PullRequest
pullRequestR user repo prid =
    query ["repos", toPathPart user, toPathPart repo, "pulls", toPathPart prid] []

createPullRequest :: Auth
                  -> Name Owner
                  -> Name Repo
                  -> CreatePullRequest
                  -> IO (Either Error PullRequest)
createPullRequest auth user repo cpr =
    executeRequest auth $ createPullRequestR user repo cpr

-- | Create a pull request.
-- See <https://developer.github.com/v3/pulls/#create-a-pull-request>
createPullRequestR :: Name Owner
                   -> Name Repo
                   -> CreatePullRequest
                   -> Request 'RW PullRequest
createPullRequestR user repo cpr =
    command Post ["repos", toPathPart user, toPathPart repo, "pulls"] (encode cpr)

-- | Update a pull request
updatePullRequest :: Auth -> Name Owner -> Name Repo -> IssueNumber -> EditPullRequest -> IO (Either Error PullRequest)
updatePullRequest auth user repo prid epr =
    executeRequest auth $ updatePullRequestR user repo prid epr

-- | Update a pull request.
-- See <https://developer.github.com/v3/pulls/#update-a-pull-request>
updatePullRequestR :: Name Owner
                   -> Name Repo
                   -> IssueNumber
                   -> EditPullRequest
                   -> Request 'RW PullRequest
updatePullRequestR user repo prid epr =
    command Patch ["repos", toPathPart user, toPathPart repo, "pulls", toPathPart prid] (encode epr)

-- | All the commits on a pull request, given the repo owner, repo name, and
-- the number of the pull request.
-- With authentification.
--
-- > pullRequestCommits' (Just ("github-username", "github-password")) "thoughtbot" "paperclip" 688
pullRequestCommits' :: Maybe Auth -> Name Owner -> Name Repo -> IssueNumber -> IO (Either Error (Vector Commit))
pullRequestCommits' auth user repo prid =
    executeRequestMaybe auth $ pullRequestCommitsR user repo prid FetchAll

-- | All the commits on a pull request, given the repo owner, repo name, and
-- the number of the pull request.
--
-- > pullRequestCommits "thoughtbot" "paperclip" 688
pullRequestCommitsIO :: Name Owner -> Name Repo -> IssueNumber -> IO (Either Error (Vector Commit))
pullRequestCommitsIO = pullRequestCommits' Nothing

-- | List commits on a pull request.
-- See <https://developer.github.com/v3/pulls/#list-commits-on-a-pull-request>
pullRequestCommitsR :: Name Owner -> Name Repo -> IssueNumber -> FetchCount -> Request k (Vector Commit)
pullRequestCommitsR user repo prid =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "pulls", toPathPart prid, "commits"] []

-- | The individual files that a pull request patches. Takes the repo owner and
-- name, plus the number assigned to the pull request.
-- With authentification.
--
-- > pullRequestFiles' (Just ("github-username", "github-password")) "thoughtbot" "paperclip" 688
pullRequestFiles' :: Maybe Auth -> Name Owner -> Name Repo -> IssueNumber -> IO (Either Error (Vector File))
pullRequestFiles' auth user repo prid =
    executeRequestMaybe auth $ pullRequestFilesR user repo prid FetchAll

-- | The individual files that a pull request patches. Takes the repo owner and
-- name, plus the number assigned to the pull request.
--
-- > pullRequestFiles "thoughtbot" "paperclip" 688
pullRequestFiles :: Name Owner -> Name Repo -> IssueNumber -> IO (Either Error (Vector File))
pullRequestFiles = pullRequestFiles' Nothing

-- | List pull requests files.
-- See <https://developer.github.com/v3/pulls/#list-pull-requests-files>
pullRequestFilesR :: Name Owner -> Name Repo -> IssueNumber -> FetchCount -> Request k (Vector File)
pullRequestFilesR user repo prid =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "pulls", toPathPart prid, "files"] []

-- | Check if pull request has been merged.
isPullRequestMerged :: Auth -> Name Owner -> Name Repo -> IssueNumber -> IO (Either Error Bool)
isPullRequestMerged auth user repo prid =
    executeRequest auth $ isPullRequestMergedR user repo prid

-- | Query if a pull request has been merged.
-- See <https://developer.github.com/v3/pulls/#get-if-a-pull-request-has-been-merged>
isPullRequestMergedR :: Name Owner -> Name Repo -> IssueNumber -> GenRequest 'MtStatus rw Bool
isPullRequestMergedR user repo prid =
    Query ["repos", toPathPart user, toPathPart repo, "pulls", toPathPart prid, "merge"] []

-- | Merge a pull request.
mergePullRequest :: Auth -> Name Owner -> Name Repo -> IssueNumber -> Maybe Text -> IO (Either Error MergeResult)
mergePullRequest auth user repo prid commitMessage =
    executeRequest auth $ mergePullRequestR user repo prid commitMessage

-- | Merge a pull request (Merge Button).
-- https://developer.github.com/v3/pulls/#merge-a-pull-request-merge-button
mergePullRequestR :: Name Owner -> Name Repo -> IssueNumber -> Maybe Text -> GenRequest 'MtStatus 'RW MergeResult
mergePullRequestR user repo prid commitMessage =
    Command Put paths (encode $ buildCommitMessageMap commitMessage)
  where
    paths = ["repos", toPathPart user, toPathPart repo, "pulls", toPathPart prid, "merge"]

    buildCommitMessageMap :: Maybe Text -> Value
    buildCommitMessageMap (Just msg) = object ["commit_message" .= msg ]
    buildCommitMessageMap Nothing    = object []

