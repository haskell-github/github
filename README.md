Github
------

The Github API v3 for Haskell.

**This is currently a read-only API.** There is a `read-write` branch with
ideas on how this can work for writing to Github, but I need ideas on how
authentication should work. *You can help* if you [let me
know](mailto:mike@mike-burns.com) how you are using this library or how you
would like to use it, and which authentication method is best for you (HTTP
basic, OAuth).

Some functions which do not require authentication are also missing; these are functions where the Github API did not work as expected. The full Github API is in beta and constantly improving.

Installation
============

In your project's cabal file:

    -- Packages needed in order to build this package.
    Build-depends:       github

Or from the command line:

    cabal install github

Example Usage
=============

See the samples in the [samples/](https://github.com/mike-burns/github/tree/master/samples) directory.

Documentation
=============

For details see the reference documentation on Hackage. Later.

Each module lines up with the hierarchy of [documentation from the Github API](http://developer.github.com/v3/).

Each function has a sample written for it.

All functions produce an `IO (Either Error a)`, where `a` is the actual thing you want. You must call the function using IO goodness, then dispatch on the possible error message. Here's an example from the samples:

    import Github.Users.Followers
    import Data.List (intercalate)
    main = do
      possibleUsers <- usersFollowing "mike-burns"
      putStrLn $ either (\error -> "Error: " ++ $ show error)
                        (intercalate "\n" . map githubUserLogin)
                        possibleUsers


Copyright
=========

Copyright 2011 Mike Burns.

Available under the BSD 3-clause license.
