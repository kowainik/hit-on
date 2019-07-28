# Changelog

`hit-on` uses [PVP Versioning][1].
The changelog is available [on GitHub][2].

### Unreleased: 0.1.0.0

* [#63](https://github.com/kowainik/hit-on/issues/63),
  [#79](https://github.com/kowainik/hit-on/issues/79):
  Implement `hit status` command with pretty output.
* [#82](https://github.com/kowainik/hit-on/issues/82):
  Add `--force` flag to `hit fix` command.
* [#85](https://github.com/kowainik/hit-on/issues/85):
  Make `hit commit` command take the commit name from the corresponding issue
  name, if it is currently applicable (the branch name has the info about issue
  number).
* Bump up to GHC 8.6.5.
* Bump up to `relude-0.5.0`.

### 0.0.0

* Initially created.

[1]: https://pvp.haskell.org
[2]: https://github.com/kowainik/hit-on/releases
