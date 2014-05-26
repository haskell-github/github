-- | The Github issue comments API from
-- <http://developer.github.com/v3/issues/comments/>.
module Github.Issues.Comments (
 comment
,comments
,comments'
,createComment
,editComment
,module Github.Data
) where

import Github.Data
import Github.Private

-- | A specific comment, by ID.
--
-- > comment "thoughtbot" "paperclip" 1468184
comment :: String -> String -> Int -> IO (Either Error IssueComment)
comment user reqRepoName reqCommentId =
  githubGet ["repos", user, reqRepoName, "issues", "comments", show reqCommentId]

-- | All comments on an issue, by the issue's number.
--
-- > comments "thoughtbot" "paperclip" 635
comments :: String -> String -> Int -> IO (Either Error [IssueComment])
comments user reqRepoName reqIssueNumber =
  githubGet ["repos", user, reqRepoName, "issues", show reqIssueNumber, "comments"]

-- | All comments on an issue, by the issue's number, using authentication.
--
-- > comments' (GithubUser (user, password)) "thoughtbot" "paperclip" 635
comments' :: Maybe GithubAuth -> String -> String -> Int -> IO (Either Error [IssueComment])
comments' auth user reqRepoName reqIssueNumber =
  githubGet' auth ["repos", user, reqRepoName, "issues", show reqIssueNumber, "comments"]



-- |
-- Create a new comment.
--
-- > createComment (GithubUser (user, password)) user repo issue
-- >  "some words"
createComment :: GithubAuth -> String -> String -> Int -> String
            -> IO (Either Error Comment)
createComment auth user repo iss body =
  githubPost auth
  ["repos", user, repo, "issues", show iss, "comments"] (NewComment body)


-- |
-- Edit a comment.
--
-- > editComment (GithubUser (user, password)) user repo commentid
-- >  "new words"
editComment :: GithubAuth -> String -> String -> Int -> String
            -> IO (Either Error Comment)
editComment auth user repo commid body =
  githubPatch auth ["repos", user, repo, "issues", "comments", show commid]
  (EditComment body)
