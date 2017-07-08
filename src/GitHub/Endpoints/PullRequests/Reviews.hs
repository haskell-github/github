-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The reviews API as described on <http://developer.github.com/v3/pulls/reviews/>.
module GitHub.Endpoints.PullRequests.Reviews
  ( reviewsForR
  , reviewsFor
  , reviewsFor'
  , reviewForR
  , reviewFor
  , reviewFor'
  , reviewCommentsForR
  , reviewCommentsFor
  , reviewCommentsFor'
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
reviewsForR
  :: Name Owner
  -> Name Repo
  -> Id PullRequest
  -> FetchCount
  -> Request k (Vector Review)
reviewsForR owner repo prid =
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
-- > reviewsFor "thoughtbot" "paperclip" (Id 101)
reviewsFor :: Name Owner
           -> Name Repo
           -> Id PullRequest
           -> IO (Either Error (Vector Review))
reviewsFor owner repo prid =
  executeRequest' $ reviewsForR owner repo prid FetchAll


-- | All reviews for a pull request given the repo owner, repo name and the pull
-- request id. With authentication.
--
-- > reviewsFor' (Just ("github-username", "github-password")) "thoughtbot" "paperclip" (Id 101)
reviewsFor'
  :: Maybe Auth
  -> Name Owner
  -> Name Repo
  -> Id PullRequest
  -> IO (Either Error (Vector Review))
reviewsFor' auth owner repo pr =
  executeRequestMaybe auth $ reviewsForR owner repo pr FetchAll


-- | Query a single pull request review.
-- see <https://developer.github.com/v3/pulls/reviews/#get-a-single-review>
reviewForR :: Name Owner
           -> Name Repo
           -> Id PullRequest
           -> Id Review
           -> Request k Review
reviewForR owner repo prid rid =
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
-- > reviewFor "thoughtbot" "factory_girl" (Id 301819) (Id 332)
reviewFor
  :: Name Owner
  -> Name Repo
  -> Id PullRequest
  -> Id Review
  -> IO (Either Error Review)
reviewFor owner repo prid rid =
  executeRequest' $ reviewForR owner repo prid rid


-- | A detailed review on a pull request given the repo owner, repo name, pull
-- request id and review id. With authentication.
--
-- > reviewFor' (Just ("github-username", "github-password"))
-- "thoughtbot" "factory_girl" (Id 301819) (Id 332)
reviewFor'
  :: Maybe Auth
  -> Name Owner
  -> Name Repo
  -> Id PullRequest
  -> Id Review
  -> IO (Either Error Review)
reviewFor' auth owner repo prid rid =
  executeRequestMaybe auth $ reviewForR owner repo prid rid


-- | Query the comments for a single pull request review.
-- see <https://developer.github.com/v3/pulls/reviews/#get-comments-for-a-single-review>
reviewCommentsForR
  :: Name Owner
  -> Name Repo
  -> Id PullRequest
  -> Id Review
  -> Request k [ReviewComment]
reviewCommentsForR owner repo prid rid =
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
-- > reviewCommentsFor "thoughtbot" "factory_girl" (Id 301819) (Id 332)
reviewCommentsFor
  :: Name Owner
  -> Name Repo
  -> Id PullRequest
  -> Id Review
  -> IO (Either Error [ReviewComment])
reviewCommentsFor owner repo prid rid =
  executeRequest' $ reviewCommentsForR owner repo prid rid


-- | All comments for a review on a pull request given the repo owner, repo
-- name, pull request id and review id. With authentication.
--
-- > reviewCommentsFor' (Just ("github-username", "github-password")) "thoughtbot" "factory_girl" (Id 301819) (Id 332)
reviewCommentsFor'
  :: Maybe Auth
  -> Name Owner
  -> Name Repo
  -> Id PullRequest
  -> Id Review
  -> IO (Either Error [ReviewComment])
reviewCommentsFor' auth owner repo prid rid =
  executeRequestMaybe auth $ reviewCommentsForR owner repo prid rid
