-- | The Github issue comments API from
-- <http://developer.github.com/v3/issues/comments/>.
module Github.Issues.Comments (
 comment
,comments
,comments'

-- * Modifying Comments
-- |
-- Only authenticated users may create and edit comments.
,GithubAuth(..)

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
comment user repoName commentId =
  githubGet ["repos", user, repoName, "issues", "comments", show commentId]

-- | All comments on an issue, by the issue's number.
--
-- > comments "thoughtbot" "paperclip" 635
comments :: String -> String -> Int -> IO (Either Error [IssueComment])
comments user repoName issueNumber =
  githubGet ["repos", user, repoName, "issues", show issueNumber, "comments"]

-- | All comments on an issue, by the issue's number, using authentication.
--
-- > comments' (GithubUser (user, password)) "thoughtbot" "paperclip" 635
comments' :: Maybe GithubAuth -> String -> String -> Int -> IO (Either Error [IssueComment])
comments' auth user repoName issueNumber =
  githubGet' auth ["repos", user, repoName, "issues", show issueNumber, "comments"]



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
