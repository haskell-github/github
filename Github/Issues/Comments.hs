-- | The Github issue comments API from
-- <http://developer.github.com/v3/issues/comments/>.
module Github.Issues.Comments (
 comment
,comments
,module Github.Data
) where

import Github.Data
import Github.Private

-- | A specific comment, by ID.
--
-- > comment def "thoughtbot" "paperclip" 1468184
comment :: GithubConfig -> String -> String -> Int -> IO (Either Error IssueComment)
comment c user repoName commentId =
  githubGet c ["repos", user, repoName, "issues", "comments", show commentId]

-- | All comments on an issue, by the issue's number.
--
-- > comments def "thoughtbot" "paperclip" 635
comments :: GithubConfig -> String -> String -> Int -> IO (Either Error [IssueComment])
comments c user repoName issueNumber =
  githubGet c ["repos", user, repoName, "issues", show issueNumber, "comments"]
