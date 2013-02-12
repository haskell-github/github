-- | The Github issue comments API from
-- <http://developer.github.com/v3/issues/comments/>.
module Github.Issues.Comments (
 comment
,comments
,comments'
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

