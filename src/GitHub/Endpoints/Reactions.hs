-- |
-- The Reactions API as described at
-- <https://docs.github.com/en/rest/reactions/reactions?apiVersion=2022-11-28>.
module GitHub.Endpoints.Reactions (
  issueReactionsR,
  createIssueReactionR,
  deleteIssueReactionR,
  commentReactionsR,
  createCommentReactionR,
  deleteCommentReactionR,
  module GitHub.Data,
) where

import GitHub.Data
import GitHub.Internal.Prelude
import Prelude ()

-- | List reactions for an issue.
-- See <https://docs.github.com/en/rest/reactions/reactions?apiVersion=2022-11-28#list-reactions-for-an-issue>
issueReactionsR :: Name Owner -> Name Repo -> Id Issue -> FetchCount -> Request k (Vector Reaction)
issueReactionsR owner repo iid =
  pagedQuery ["repos", toPathPart owner, toPathPart repo, "issues", toPathPart iid, "reactions"] []

-- | Create reaction for an issue comment.
-- See <https://docs.github.com/en/rest/reactions/reactions?apiVersion=2022-11-28#create-reaction-for-an-issue>
createIssueReactionR :: Name Owner -> Name Repo -> Id Issue -> ReactionContent -> Request 'RW Reaction
createIssueReactionR owner repo iid content =
    command Post parts (encode $ NewReaction content)
  where
    parts = ["repos", toPathPart owner, toPathPart repo, "issues", toPathPart iid, "reactions"]

-- | Delete an issue comment reaction.
-- See <https://docs.github.com/en/rest/reactions/reactions?apiVersion=2022-11-28#delete-an-issue-reaction>
deleteIssueReactionR :: Name Owner -> Name Repo -> Id Issue -> Id Reaction -> GenRequest 'MtUnit 'RW ()
deleteIssueReactionR owner repo iid rid =
    Command Delete parts mempty
  where
    parts = ["repos", toPathPart owner, toPathPart repo, "issues", toPathPart iid, "reactions", toPathPart rid]

-- | List reactions for an issue comment.
-- See <https://docs.github.com/en/rest/reactions/reactions?apiVersion=2022-11-28#list-reactions-for-an-issue-comment>
commentReactionsR :: Name Owner -> Name Repo -> Id Comment -> FetchCount -> Request k (Vector Reaction)
commentReactionsR owner repo cid =
  pagedQuery ["repos", toPathPart owner, toPathPart repo, "issues", "comments", toPathPart cid, "reactions"] []

-- | Create reaction for an issue comment.
-- See https://docs.github.com/en/rest/reactions/reactions?apiVersion=2022-11-28#create-reaction-for-an-issue-comment
createCommentReactionR :: Name Owner -> Name Repo -> Id Comment -> ReactionContent -> Request 'RW Reaction
createCommentReactionR owner repo cid content =
    command Post parts (encode $ NewReaction content)
  where
    parts = ["repos", toPathPart owner, toPathPart repo, "issues", "comments", toPathPart cid, "reactions"]

-- | Delete an issue comment reaction.
-- See <https://docs.github.com/en/rest/reactions/reactions?apiVersion=2022-11-28#delete-an-issue-comment-reaction>
deleteCommentReactionR :: Name Owner -> Name Repo -> Id Comment -> Id Reaction -> GenRequest 'MtUnit 'RW ()
deleteCommentReactionR owner repo cid rid =
    Command Delete parts mempty
  where
    parts = ["repos", toPathPart owner, toPathPart repo, "issues", "comments", toPathPart cid, "reactions", toPathPart rid]
