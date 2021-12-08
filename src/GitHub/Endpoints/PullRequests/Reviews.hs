-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The reviews API as described on <http://developer.github.com/v3/pulls/reviews/>.
module GitHub.Endpoints.PullRequests.Reviews
    ( pullRequestReviewsR
    , pullRequestReviewR
    , pullRequestReviewCommentsR
    , module GitHub.Data
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

-- | List reviews for a pull request.
-- See <https://developer.github.com/v3/pulls/reviews/#list-reviews-on-a-pull-request>
pullRequestReviewsR
    :: Name Owner
    -> Name Repo
    -> IssueNumber
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

-- | Query a single pull request review.
-- see <https://developer.github.com/v3/pulls/reviews/#get-a-single-review>
pullRequestReviewR
    :: Name Owner
    -> Name Repo
    -> IssueNumber
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

-- | Query the comments for a single pull request review.
-- see <https://developer.github.com/v3/pulls/reviews/#get-comments-for-a-single-review>
pullRequestReviewCommentsR
    :: Name Owner
    -> Name Repo
    -> IssueNumber
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
