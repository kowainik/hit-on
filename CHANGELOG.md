# Changelog

`hit-on` uses [PVP Versioning][1].
The changelog is available [on GitHub][2].

### Unreleased: 0.2.0.0

* [#65](https://github.com/kowainik/hit-on/issues/55):
  Add `--issue` option to `hit new` command to create issue.
  (by [@bangn](https://github.com/bangn)).
* [#127](https://github.com/kowainik/hit-on/issues/127):
  Assign user to issue on `hit new --issue` command.
  (by [@chshersh](https://github.com/chshersh)).
* [#15](https://github.com/kowainik/hit-on/issues/15):
  When the new branch for the issue is created, add the user as an assignee to
  this issue.
  (by [@vrom911](https://github.com/vrom911)).
* [#125](https://github.com/kowainik/hit-on/pull/125):
  Move to the newer `relude-0.6.0.0`.
  (by [@vrom911](https://github.com/vrom911)).
* [#133](https://github.com/kowainik/hit-on/pull/133):
  Move to GHC-8.8.1.

### 0.1.0.0 — Aug 3, 2019

* [#85](https://github.com/kowainik/hit-on/issues/85):
  Make `hit commit` command take the commit name from the corresponding issue
  name, if it is currently applicable (the branch name has the info about issue
  number)
  (by [@vrom911](https://github.com/vrom911)).
* [#63](https://github.com/kowainik/hit-on/issues/63),
  [#79](https://github.com/kowainik/hit-on/issues/79),
  [#86](https://github.com/kowainik/hit-on/issues/86):
  Implement `hit status` command with pretty output
  (by [@chshersh](https://github.com/chshersh)).
* [#72](https://github.com/kowainik/hit-on/issues/72):
  Allow `hit new` command to receive branch names as long as issue numbers
  (by [@vrom911](https://github.com/vrom911)).
* [#89](https://github.com/kowainik/hit-on/issues/89):
  Add `-p|push` and `-f|force` flags to `hit commit` command
  (by [@vrom911](https://github.com/vrom911)).
* [#81](https://github.com/kowainik/hit-on/issues/81):
  Implement `hit diff` command with pretty diff
  (by [@chshersh](https://github.com/chshersh)).
* [#67](https://github.com/kowainik/hit-on/issues/67):
  Implement `hit stash` and `hit unstash` commands
  (by [@chshersh](https://github.com/chshersh)).
* [#77](https://github.com/kowainik/hit-on/issues/77):
  Implement `hit clear` command
  (by [@chshersh](https://github.com/chshersh)).
* [#82](https://github.com/kowainik/hit-on/issues/82):
  Add `--force` flag to `hit fix` command
  (by [@vrom911](https://github.com/vrom911)).
* [#80](https://github.com/kowainik/hit-on/issues/80):
  Add `hit uncommit` command
  (by [@vrom911](https://github.com/vrom911)).
* [#69](https://github.com/kowainik/hit-on/issues/69):
  Write autocompletion instructions
  (by [@chshersh](https://github.com/chshersh)).
* [#49](https://github.com/kowainik/hit-on/issues/49):
  Configure OSX releases on Travis CI
  (by [@chshersh](https://github.com/chshersh)).
* [#96](https://github.com/kowainik/hit-on/issues/96):
  Add table of all commands to README
  (by [@vrom911](https://github.com/vrom911)).
* [#35](https://github.com/kowainik/hit-on/issues/35):
  Write unit tests for URL parsing functions
  (by [@kahlil29](https://github.com/kahlil29)).
* Bump up to GHC 8.6.5
  (by [@chshersh](https://github.com/chshersh)).
* Bump up to `relude-0.5.0`
  (by [@chshersh](https://github.com/chshersh)).

### 0.0.0

* Initially created.

[1]: https://pvp.haskell.org
[2]: https://github.com/kowainik/hit-on/releases
