Github
------

[![Build Status](https://travis-ci.org/phadej/github.svg?branch=master)](https://travis-ci.org/phadej/github)
[![Hackage](https://img.shields.io/hackage/v/github.svg)][hackage]
[![Stackage LTS 5](http://stackage.org/package/github/badge/lts-5)](http://stackage.org/lts-5/package/github)
[![Stackage Nightly](http://stackage.org/package/github/badge/nightly)](http://stackage.org/nightly/package/github)

The Github API v3 for Haskell.

Some functions are missing; these are functions where the Github API did
not work as expected. The full Github API is in beta and constantly
improving.

Installation
============

In your project's cabal file:

```cabal
-- Packages needed in order to build this package.
Build-depends:       github
```

Or from the command line:

```sh
cabal install github
```

Example Usage
=============

See the samples in the
[samples/](https://github.com/fpco/github/tree/master/samples) directory.

Documentation
=============

For details see the reference [documentation on Hackage][hackage].

Each module lines up with the hierarchy of
[documentation from the Github API](http://developer.github.com/v3/).

Request functions (ending with `R`) construct a data type with can be executed
in `IO` by `executeRequest` functions. They are all listed in the root `GitHub`
module.

IO functions produce an `IO (Either Error a)`, where `a` is the actual thing
you want. You must call the function using IO goodness, then dispatch on the
possible error message. Here's an example from the samples:

Many function have samples under
[`samples/`](https://github.com/phadej/github/tree/master/samples) directory.

```hs
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude.Compat

import Data.Text         (Text, pack)
import Data.Text.IO as T (putStrLn)
import Data.Monoid       ((<>))

import qualified GitHub.Endpoints.Users.Followers as GitHub

main :: IO ()
main = do
    possibleUsers <- GitHub.usersFollowing "mike-burns"
    T.putStrLn $ either (("Error: " <>) . pack . show)
                        (foldMap ((<> "\n") . formatUser))
                        possibleUsers

formatUser :: GitHub.SimpleUser -> Text
formatUser = GitHub.untagName . GitHub.simpleUserLogin
```

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

Copyright 2011-2012 Mike Burns.
Copyright 2013-2015 John Wiegley.
Copyright 2016 Oleg Grenrus.

Available under the BSD 3-clause license.

[hackage]: http://hackage.haskell.org/package/github "Hackage"
