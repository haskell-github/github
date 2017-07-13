-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The reviews API as described on <http://developer.github.com/v3/pulls/reviews/>.
module GitHub.Endpoints.PullRequests.Reviews
    ( pullRequestReviewsR
    , pullRequestReviews
    , pullRequestReviews'
    , pullRequestReviewR
    , pullRequestReview
    , pullRequestReview'
    , pullRequestReviewCommentsR
    , pullRequestReviewCommentsIO
    , pullRequestReviewCommentsIO'
    , module GitHub.Data
    ) where

import GitHub.Data
import GitHub.Data.Id (Id)
import GitHub.Internal.Prelude
import GitHub.Request
       (Request, executeRequest', executeRequestMaybe)
import Prelude ()

-- | List reviews for a pull request.
-- See <https://developer.github.com/v3/pulls/reviews/#list-reviews-on-a-pull-request>
pullRequestReviewsR
    :: Name Owner
    -> Name Repo
    -> Id PullRequest
    -> FetchCount
    -> Request k (Vector Review)
pullRequestReviewsR owner repo prid =
    pagedQuery
        [ "repos"
        , toPathPart owner
        , toPathPart repo
        , "pulls"
        , toPathPart prid
        , "reviews"
        ]
        []

-- | All reviews for a pull request given the repo owner, repo name and the pull
-- request id.
--
-- > pullRequestReviews "thoughtbot" "paperclip" (Id 101)
pullRequestReviews
    :: Name Owner
    -> Name Repo
    -> Id PullRequest
    -> IO (Either Error (Vector Review))
pullRequestReviews owner repo prid =
    executeRequest' $ pullRequestReviewsR owner repo prid FetchAll

-- | All reviews for a pull request given the repo owner, repo name and the pull
-- request id. With authentication.
--
-- > pullRequestReviews' (Just ("github-username", "github-password")) "thoughtbot" "paperclip" (Id 101)
pullRequestReviews'
    :: Maybe Auth
    -> Name Owner
    -> Name Repo
    -> Id PullRequest
    -> IO (Either Error (Vector Review))
pullRequestReviews' auth owner repo pr =
    executeRequestMaybe auth $ pullRequestReviewsR owner repo pr FetchAll

-- | Query a single pull request review.
-- see <https://developer.github.com/v3/pulls/reviews/#get-a-single-review>
pullRequestReviewR
    :: Name Owner
    -> Name Repo
    -> Id PullRequest
    -> Id Review
    -> Request k Review
pullRequestReviewR owner repo prid rid =
    query
        [ "repos"
        , toPathPart owner
        , toPathPart repo
        , "pulls"
        , toPathPart prid
        , "reviews"
        , toPathPart rid
        ]
        []

-- | A detailed review on a pull request given the repo owner, repo name, pull
-- request id and review id.
--
-- > pullRequestReview "thoughtbot" "factory_girl" (Id 301819) (Id 332)
pullRequestReview
    :: Name Owner
    -> Name Repo
    -> Id PullRequest
    -> Id Review
    -> IO (Either Error Review)
pullRequestReview owner repo prid rid =
    executeRequest' $ pullRequestReviewR owner repo prid rid

-- | A detailed review on a pull request given the repo owner, repo name, pull
-- request id and review id. With authentication.
--
-- > pullRequestReview' (Just ("github-username", "github-password"))
-- "thoughtbot" "factory_girl" (Id 301819) (Id 332)
pullRequestReview'
    :: Maybe Auth
    -> Name Owner
    -> Name Repo
    -> Id PullRequest
    -> Id Review
    -> IO (Either Error Review)
pullRequestReview' auth owner repo prid rid =
    executeRequestMaybe auth $ pullRequestReviewR owner repo prid rid

-- | Query the comments for a single pull request review.
-- see <https://developer.github.com/v3/pulls/reviews/#get-comments-for-a-single-review>
pullRequestReviewCommentsR
    :: Name Owner
    -> Name Repo
    -> Id PullRequest
    -> Id Review
    -> Request k [ReviewComment]
pullRequestReviewCommentsR owner repo prid rid =
    query
        [ "repos"
        , toPathPart owner
        , toPathPart repo
        , "pulls"
        , toPathPart prid
        , "reviews"
        , toPathPart rid
        , "comments"
        ]
        []

-- | All comments for a review on a pull request given the repo owner, repo
-- name, pull request id and review id.
--
-- > pullRequestReviewComments "thoughtbot" "factory_girl" (Id 301819) (Id 332)
pullRequestReviewCommentsIO
    :: Name Owner
    -> Name Repo
    -> Id PullRequest
    -> Id Review
    -> IO (Either Error [ReviewComment])
pullRequestReviewCommentsIO owner repo prid rid =
    executeRequest' $ pullRequestReviewCommentsR owner repo prid rid

-- | All comments for a review on a pull request given the repo owner, repo
-- name, pull request id and review id. With authentication.
--
-- > pullRequestReviewComments' (Just ("github-username", "github-password")) "thoughtbot" "factory_girl" (Id 301819) (Id 332)
pullRequestReviewCommentsIO'
    :: Maybe Auth
    -> Name Owner
    -> Name Repo
    -> Id PullRequest
    -> Id Review
    -> IO (Either Error [ReviewComment])
pullRequestReviewCommentsIO' auth owner repo prid rid =
    executeRequestMaybe auth $ pullRequestReviewCommentsR owner repo prid rid
