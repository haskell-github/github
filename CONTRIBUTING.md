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
-- <https://developer.github.com/v3/rate_limit/#get-your-current-rate-limit-status>
rateLimitR :: Request k RateLimit
rateLimitR = query ["rate_limit"] []
```

Also re-export endpoints from the top `GitHub` module. *Note:* only `R` variants, not `IO`.

Miscellaneous
-------------

* **Don't** edit `CHANGELOG.md`, it will only conflict.
* **Don't** edit package version.
* The codebase is not uniform in style, don't make it worse.
