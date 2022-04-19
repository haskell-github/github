Contributing
============

When adding a new endpoint
--------------------------

```haskell
-- | The title, as in the GitHub API docs.
-- <url to the docs>
endpointR :: Request k EndpointResult
endpointR = query ["endpoint"] []
```

For example:

```haskell
-- | Get your current rate limit status.
-- <https://docs.github.com/en/rest/reference/rate-limit#get-your-current-rate-limit-status>
rateLimitR :: Request k RateLimit
rateLimitR = query ["rate_limit"] []
```

Also re-export endpoints from the top `GitHub` module. *Note:* only `R` variants, not `IO`.

Testing
-------

When adding new functionality, cover it by a test case in:

    spec/

or a demonstration added to:

    samples/github-samples.cabal

Miscellaneous
-------------

* **Don't** edit `CHANGELOG.md`, it will only conflict.
* **Don't** edit package version.
* The codebase is not uniform in style, don't make it worse.
