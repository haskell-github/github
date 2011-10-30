Github
------

The Github API v3 for Haskell.

Installation
============

In your project's cabal file:

    -- Packages needed in order to build this package.
    Build-depends:       github

Or from the command line:

    cabal install github

Example Usage
=============

    import qualified Github.Repos.Commits as Github

    main = do
      possibleCommits <- Github.commitsFor "thoughtbot" "paperclip"
      case possibleCommits of
        (Left error)    -> putStrLn $ "Error: " ++ (show error)
        (Right commits) -> putStrLn $ show $ map formatCommit commits

    formatCommit :: Commit -> String
    formatCommit commit =
      "commit " ++ (Github.commitSha commit) ++
        "\nAuthor: " ++ (formatAuthor author) ++
        "\nDate:   " ++ (show $ Github.authorDate author) ++
        "\n\n\t" ++ (Github.commitMessage commit)
      where author = Github.commitAuthor commit

    formatAuthor :: Author -> String
    formatAuthor author =
      (Github.authorName author) ++ " <" ++ (Github.authorEmail author) ++ ">"

Documentation
=============

For details see the reference documentation on Hackage.

Copyright
=========

Copyright 2011 Mike Burns.

Available under the BSD 3-clause license.
