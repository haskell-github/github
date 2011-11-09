module CommentOnCommit where

import qualified Github.Repos.Commits as Github

-- blocked on auth
main = do
  let commentPoster = Github.postCommentOn "mike-burns" "github" "746779d28dbbeece2593ba37a30a1b457edf3f6e"
      comment = Github.NewComment {
         Github.newCommentBody       = "Good call"
        ,Github.newCommentLineNumber = 20
        ,Github.newCommentPath       = "Github/Data.hs"
        ,Github.newCommentPosition   = 20
        ,Github.newCommentCommitId   = "746779d28dbbeece2593ba37a30a1b457edf3f6e"
      }
  didItWork <- commentPoster comment
  putStrLn $ either (\error   -> "Error: "  ++ (show error))
                    (\comment -> "Posted: " ++ (show comment))
                    didItWork
