# Changelog

`hit-on` uses [PVP Versioning][1].
The changelog is available [on GitHub][2].

### Unreleased: 0.1.0.0

* [#63](https://github.com/kowainik/hit-on/issues/63),
  [#79](https://github.com/kowainik/hit-on/issues/79),
  [#86](https://github.com/kowainik/hit-on/issues/86):
  Implement `hit status` command with pretty output.
* [#67](https://github.com/kowainik/hit-on/issues/67):
  Implement `hit stash` and `hit unstash` commands.
* [#77](https://github.com/kowainik/hit-on/issues/77):
  Implement `hit clear` command.
* [#81](https://github.com/kowainik/hit-on/issues/81):
  Implement `hit diff` command with pretty diff.
* [#82](https://github.com/kowainik/hit-on/issues/82):
  Add `--force` flag to `hit fix` command.
* [#85](https://github.com/kowainik/hit-on/issues/85):
  Make `hit commit` command take the commit name from the corresponding issue
  name, if it is currently applicable (the branch name has the info about issue
  number).
* [#89](https://github.com/kowainik/hit-on/issues/89):
  Add `-p|push` and `-f|force` flags to `hit commit` command.
* [#80](https://github.com/kowainik/hit-on/issues/80):
  Add `hit uncommit` command.
* [#69](https://github.com/kowainik/hit-on/issues/69):
  Write autocompletion instructions.
* [#72](https://github.com/kowainik/hit-on/issues/72):
  Allow `hit new` command to receive branch names as long as issue numbers.
* Bump up to GHC 8.6.5.
* Bump up to `relude-0.5.0`.

### 0.0.0

* Initially created.

[1]: https://pvp.haskell.org
[2]: https://github.com/kowainik/hit-on/releases
