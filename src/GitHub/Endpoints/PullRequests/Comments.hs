-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The pull request review comments API as described at
-- <http://developer.github.com/v3/pulls/comments/>.
module GitHub.Endpoints.PullRequests.Comments (
    pullRequestCommentsIO,
    pullRequestCommentsR,
    pullRequestCommentsWithReactionsIO,
    pullRequestCommentsWithReactionsR,
    pullRequestComment,
    pullRequestCommentR,
    createPullComment,
    createPullCommentR,
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

-- | All the comments on a pull request with the given ID.
--
-- > pullRequestCommentsIO "thoughtbot" "factory_girl" (IssueNumber 256)
pullRequestCommentsIO :: Name Owner -> Name Repo -> IssueNumber -> IO (Either Error (Vector Comment))
pullRequestCommentsIO user repo prid =
    executeRequest' $ pullRequestCommentsR user repo prid FetchAll

-- | List comments on a pull request.
-- See <https://developer.github.com/v3/pulls/comments/#list-comments-on-a-pull-request>
pullRequestCommentsR :: Name Owner -> Name Repo -> IssueNumber -> FetchCount -> Request k (Vector Comment)
pullRequestCommentsR user repo prid =
    pagedQuery ["repos", toPathPart user, toPathPart repo, "pulls", toPathPart prid, "comments"] []

-- | All the comments and their reactions in a pull request with a given ID.
--
-- > pullRequestCommentsWithReactionsIO "thoughtbot" "factory_girl" (IssueNumber 256)
pullRequestCommentsWithReactionsIO :: Name Owner -> Name Repo -> IssueNumber -> IO (Either Error (Vector Comment))
pullRequestCommentsWithReactionsIO user repo prid =
    executeRequest' $ pullRequestCommentsWithReactionsR user repo prid FetchAll

-- | List comments on a pull request as well as the reactions.
-- See <https://developer.github.com/v3/reactions>
pullRequestCommentsWithReactionsR :: Name Owner -> Name Repo -> IssueNumber -> FetchCount -> GenRequest 'MtReactions k (Vector Comment)
pullRequestCommentsWithReactionsR user repo prid =
    PagedQuery ["repos", toPathPart user, toPathPart repo, "pulls", toPathPart prid, "comments"] []

-- | One comment on a pull request, by the comment's ID.
--
-- > pullRequestComment "thoughtbot" "factory_girl" (Id 301819)
pullRequestComment :: Name Owner -> Name Repo -> Id Comment -> IO (Either Error Comment)
pullRequestComment user repo cid =
    executeRequest' $ pullRequestCommentR user repo cid

-- | Query a single comment.
-- See <https://developer.github.com/v3/pulls/comments/#get-a-single-comment>
pullRequestCommentR :: Name Owner -> Name Repo -> Id Comment -> Request k Comment
pullRequestCommentR user repo cid =
    query ["repos", toPathPart user, toPathPart repo, "pulls", "comments", toPathPart cid] []

-- | Create a new comment.
--
-- > createPullComment (BasicAuth "github-username" "github-password") user repo issue commit path position
-- >  "some words"
createPullComment :: Auth -> Name Owner -> Name Repo -> IssueNumber -> Text -> Text -> Int -> Text
            -> IO (Either Error Comment)
createPullComment auth user repo iss commit path position body =
    executeRequest auth $ createPullCommentR user repo iss commit path position body

-- | Create a comment.
--
-- See <https://developer.github.com/v3/pulls/comments/#create-a-comment>
createPullCommentR :: Name Owner -> Name Repo -> IssueNumber -> Text -> Text -> Int -> Text -> Request 'RW Comment
createPullCommentR user repo iss commit path position body =
    command Post parts (encode $ NewPullComment commit path position body)
  where
    parts = ["repos", toPathPart user, toPathPart repo, "pulls", toPathPart iss, "comments"]
