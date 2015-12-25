Github
------

The Github API v3 for Haskell.

Some functions are missing; these are functions where the Github API did
not work as expected. The full Github API is in beta and constantly
improving.

Installation
============

In your project's cabal file:

    -- Packages needed in order to build this package.
    Build-depends:       github

Or from the command line:

    cabal install github

Example Usage
=============

See the samples in the
[samples/](https://github.com/fpco/github/tree/master/samples) directory.

Documentation
=============

For details see the reference documentation on Hackage.

Each module lines up with the hierarchy of
[documentation from the Github API](http://developer.github.com/v3/).

Each function has a sample written for it.

All functions produce an `IO (Either Error a)`, where `a` is the actual thing
you want. You must call the function using IO goodness, then dispatch on the
possible error message. Here's an example from the samples:

    import qualified Github.Users.Followers as Github
    import Data.List (intercalate)

    main = do
      possibleUsers <- Github.usersFollowing "mike-burns"
      putStrLn $ either (("Error: "++) . show)
                        (intercalate "\n" . map formatUser)
                        possibleUsers

    formatUser = Github.githubOwnerLogin

API -> Module
============

<!---
## Activity
parseEvent? Don't know what support there is.

## Enterprise 2.3

No support? Not sure.

## Miscellaneous

### Emojis
### Gitignore
### Licenses
### Markdown
### Meta
### Rate Limit
--->

## Gists

[Gists module](https://github.com/jwiegley/github/blob/master/Github/Gists.hs)

- Comments on gist by gist id
- Specific comment by comment id

## Git Data

[Git Data](https://github.com/jwiegley/github/tree/master/Github/GitData)

- Blobs
  - user/repo and commit sha
- Commits
  - user/repo and commit sha
- References
  - single reference by ref name
  - history of references for a user/repo
  - references by user/repo, limited by namespace (you can get tags by specifying "tags" here)
- Trees

## Issues

[Issues](https://github.com/jwiegley/github/blob/master/Github/Issues.hs)

- Create issue
- Edit issue
- Get issues for repo

## Organizations

[Orgs](https://github.com/jwiegley/github/tree/master/Github/Organizations)

- get members by organization

## Pull Requests

[Pull Requests](https://github.com/jwiegley/github/tree/master/Github/PullRequests)

- Review Comments by PR id or comment id


## Repositories

[Repos](https://github.com/jwiegley/github/tree/master/Github/Repos)

- repos by user
- repos by organization

## Search

[Search](https://github.com/jwiegley/github/blob/master/Github/Search.hs)

- Repo search w/ authentication
- Repo search w/o auth
- Code search w/ auth
- Code search w/o auth

## Users

[Users](https://github.com/jwiegley/github/blob/master/Github/Users.hs)

- by name, with auth
- by name, with password
- by name, public info

See `DetailedOwner` to know what data could be provided.

Test setup
==========

To run integration part of tests, you'll need [github access token](https://github.com/settings/tokens/new)
Token is needed, because unauthorised access is highly limited.
It's enough to add only basic read access for public information.

With `travis encrypt --org --repo yournick/github "GITHUB_TOKEN=yourtoken"` command you get a secret,
you can use in your travis setup to run the test-suite there.

Contributions
=============

Please see
[CONTRIBUTING.md](https://github.com/fpco/github/blob/master/CONTRIBUTING.md)
for details on how you can help.

Copyright
=========

Copyright 2011, 2012 Mike Burns.
Copyright 2013-2014 John Wiegley.

Available under the BSD 3-clause license.
