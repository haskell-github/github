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
      commits <- Github.commitsFor "thoughtbot" "paperclip"
      putStrLn $ map formatCommit commits

    formatCommit :: Commit -> String
    formatCommit commit =
      "commit " ++ (Github.commitSha commit) ++
        "\nAuthor: " ++ (formatAuthor $ Github.commitAuthor commit) ++
        "\nDate:   " ++ (show $ Github.commitCommiterDate commit) ++
        "\n\n\t" ++ (Github.commitMessage commit)

    formatAuthor :: Author -> String
    formatAuthor author =
      (Github.authorName author) ++ " <" ++ (Github.authorEmail) ++ ">

Documentation
=============

For details see the reference documentation on Hackage.

Copyright
=========

Copyright 2011 Mike Burns.

Available under the BSD 3-clause license.
