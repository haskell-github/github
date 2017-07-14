module GitHub.Data.Reviews where

import Data.Text (Text)
import GitHub.Data.Definitions (SimpleUser)
import GitHub.Data.Id (Id)
import GitHub.Data.URL (URL)
import GitHub.Internal.Prelude
import Prelude ()

data ReviewState
    = ReviewStatePending
    | ReviewStateApproved
    | ReviewStateDismissed
    | ReviewStateCommented
    | ReviewStateChangesRequested
    deriving (Show, Enum, Bounded, Eq, Ord, Generic)

instance NFData ReviewState where
    rnf = genericRnf

instance Binary ReviewState

instance FromJSON ReviewState where
    parseJSON (String "APPROVED") = pure ReviewStateApproved
    parseJSON (String "PENDING") = pure ReviewStatePending
    parseJSON (String "DISMISSED") = pure ReviewStateDismissed
    parseJSON (String "COMMENTED") = pure ReviewStateCommented
    parseJSON (String "CHANGES_REQUESTED") = pure ReviewStateChangesRequested
    parseJSON _ = fail "Unexpected ReviewState"

data Review = Review
    { reviewBody :: !Text
    , reviewCommitId :: !Text
    , reviewState :: ReviewState
    , reviewSubmittedAt :: !UTCTime
    , reviewPullRequestUrl :: !URL
    , reviewHtmlUrl :: !Text
    , reviewUser :: !SimpleUser
    , reviewId :: !(Id Review)
    } deriving (Show, Generic)

instance NFData Review where
    rnf = genericRnf

instance Binary Review

instance FromJSON Review where
    parseJSON =
        withObject "Review" $ \o ->
            Review <$> o .: "body" <*> o .: "commit_id" <*> o .: "state" <*>
            o .: "submitted_at" <*>
            o .: "pull_request_url" <*>
            o .: "html_url" <*>
            o .: "user" <*>
            o .: "id"

data ReviewComment = ReviewComment
    { reviewCommentId :: !(Id ReviewComment)
    , reviewCommentUser :: !SimpleUser
    , reviewCommentBody :: !Text
    , reviewCommentUrl :: !URL
    , reviewCommentPullRequestReviewId :: !(Id Review)
    , reviewCommentDiffHunk :: !Text
    , reviewCommentPath :: !Text
    , reviewCommentPosition :: !Int
    , reviewCommentOriginalPosition :: !Int
    , reviewCommentCommitId :: !Text
    , reviewCommentOriginalCommitId :: !Text
    , reviewCommentCreatedAt :: !UTCTime
    , reviewCommentUpdatedAt :: !UTCTime
    , reviewCommentHtmlUrl :: !URL
    , reviewCommentPullRequestUrl :: !URL
    } deriving (Show, Generic)

instance NFData ReviewComment where
    rnf = genericRnf

instance Binary ReviewComment

instance FromJSON ReviewComment where
    parseJSON =
        withObject "ReviewComment" $ \o -> ReviewComment
            <$> o .: "id"
            <*> o .: "user"
            <*> o .: "body"
            <*> o .: "url"
            <*> o .: "pull_request_review_id"
            <*> o .: "diff_hunk"
            <*> o .: "path"
            <*> o .: "position"
            <*> o .: "original_position"
            <*> o .: "commit_id"
            <*> o .: "original_commit_id"
            <*> o .: "created_at"
            <*> o .: "updated_at"
            <*> o .: "html_url"
            <*> o .: "pull_request_url"
