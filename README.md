Github
------

The Github API v3 for Haskell.

**This is currently a read-only API.** There is a `read-write` branch with
ideas on how this can work for writing to Github, but I need ideas on how
authentication should work. *You can help* if you [let me
know](mailto:mike@mike-burns.com) how you are using this library or how you
would like to use it, and which authentication method is best for you (HTTP
basic, OAuth).

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

Copyright
=========

Copyright 2011 Mike Burns.

Available under the BSD 3-clause license.
