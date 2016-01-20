module GitCreateRef where

import qualified Github.Auth as Auth
import Github.GitData.References

main :: IO ()
main = do
  let auth = Auth.OAuth "oauthtoken"
  newlyCreatedGitRef <- createReference auth "myrepo" "myowner" NewGitReference {
       newGitReferenceRef = "refs/heads/fav_tag"
      ,newGitReferenceSha = "aa218f56b14c9653891f9e74264a383fa43fefbd"
    }
  case newlyCreatedGitRef of
   (Left err) -> putStrLn $ "Error: " ++ show err
   (Right newRef) -> putStrLn . formatReference $ newRef
                        
formatReference :: GitReference -> String
formatReference ref  =
  (gitObjectSha $ gitReferenceObject ref) ++ "\t" ++ (gitReferenceRef ref)
