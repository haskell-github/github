-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The pull requests API as documented at
-- <http://developer.github.com/v3/pulls/>.
module GitHub.Endpoints.PullRequests (
    pullRequestsForR,
    pullRequestR,
    pullRequestDiffR,
    pullRequestPatchR,
    createPullRequestR,
    updatePullRequestR,
    pullRequestCommitsR,
    pullRequestFilesR,
    isPullRequestMergedR,
    mergePullRequestR,
    module GitHub.Data
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()
import Data.ByteString.Lazy (ByteString)

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

-- | Query a single pull request to obtain the diff
-- See <https://developer.github.com/v3/pulls/#get-a-single-pull-request>
pullRequestDiffR :: Name Owner -> Name Repo -> IssueNumber -> GenRequest 'MtDiff rw ByteString
pullRequestDiffR user repo prid =
    Query ["repos", toPathPart user, toPathPart repo, "pulls", toPathPart prid] []

-- | Query a single pull request to obtain the patch
-- See <https://developer.github.com/v3/pulls/#get-a-single-pull-request>
pullRequestPatchR :: Name Owner -> Name Repo -> IssueNumber -> GenRequest 'MtPatch rw ByteString
pullRequestPatchR user repo prid =
    Query ["repos", toPathPart user, toPathPart repo, "pulls", toPathPart prid] []

-- | Query a single pull request.
-- See <https://developer.github.com/v3/pulls/#get-a-single-pull-request>
pullRequestR :: Name Owner -> Name Repo -> IssueNumber -> Request k PullRequest
pullRequestR user repo prid =
    query ["repos", toPathPart user, toPathPart repo, "pulls", toPathPart prid] []

-- | Create a pull request.
-- See <https://developer.github.com/v3/pulls/#create-a-pull-request>
createPullRequestR :: Name Owner
                   -> Name Repo
                   -> CreatePullRequest
                   -> Request 'RW PullRequest
createPullRequestR user repo cpr =
    command Post ["repos", toPathPart user, toPathPart repo, "pulls"] (encode cpr)

-- | Update a pull request.
-- See <https://developer.github.com/v3/pulls/#update-a-pull-request>
updatePullRequestR :: Name Owner
                   -> Name Repo
                   -> IssueNumber
                   -> EditPullRequest
                   -> Request 'RW PullRequest
updatePullRequestR user repo prid epr =
    command Patch ["repos", toPathPart user, toPathPart repo, "pulls", toPathPart prid] (encode epr)

-- | List commits on a pull request.
-- See <https://developer.github.com/v3/pulls/#list-commits-on-a-pull-request>
pullRequestCommitsR :: Name Owner -> Name Repo -> IssueNumber -> FetchCount -> Request k (Vector Commit)
pullRequestCommitsR user repo prid =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "pulls", toPathPart prid, "commits"] []

-- | List pull requests files.
-- See <https://developer.github.com/v3/pulls/#list-pull-requests-files>
pullRequestFilesR :: Name Owner -> Name Repo -> IssueNumber -> FetchCount -> Request k (Vector File)
pullRequestFilesR user repo prid =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "pulls", toPathPart prid, "files"] []

-- | Query if a pull request has been merged.
-- See <https://developer.github.com/v3/pulls/#get-if-a-pull-request-has-been-merged>
isPullRequestMergedR :: Name Owner -> Name Repo -> IssueNumber -> GenRequest 'MtStatus rw Bool
isPullRequestMergedR user repo prid =
    Query ["repos", toPathPart user, toPathPart repo, "pulls", toPathPart prid, "merge"] []

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

