Changes for 0.4.0:

* Use http-conduit version 1.4.1.10.

Changes for 0.3.0:

* Re-instantiate the Blobs API.
* `repoDescription1 and `repoPushedAt` are a `Maybe GithubDate`.
* Add `deleteRepo`, `editRepo`, and `createRepo`.
* Private gists, issues, organizations, pull requests, and users.
* Lock down `tls` and `tls-extra` instead of keeping up with the
  ever-changing `http-conduit` package.
* Features by [Pavel Ryzhov](https://github.com/paulrzcz) and [Simon Hengel](https://github.com/sol).

Changes for 0.2.1:

* Expand the unordered-containers dependency to anything in 0.1.x .

Changes for 0.2.0:

* `milestoneDueOn` and `repoLanguage` are now `Maybe` types.
* Introduce `GithubOwner` as the sum type for a `GithubUser` or `GithubOrganization`. Everything that once produced a `GithubUser` now produces a `GithubOwner`. All record accessors have changed their names
* Similar to `GithubOwner`, introduce `DetailedOwner`, which can be a `DetailedUser` or a `DetailedOrganization`. All record accessors have changed their names
* An `HTTPConnectionError` now composes `SomeException` instead of `IOException`. All exceptions raised by the underlying http-conduit library are encapulated there.
* The `githubIssueClosedBy` function now produces a `Maybe GithubOwner`.
* Remove the Blobs API, as it is broken upstream.
* Bugs found and squashed thanks to [Joey Hess](https://github.com/joeyh) and [Simon Hengel](https://github.com/sol).
