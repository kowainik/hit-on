# Hit On

![hit-on logo](https://user-images.githubusercontent.com/4276606/53816638-d86e4a00-3f9e-11e9-83ab-74032363292f.png)

[![GitHub CI](https://github.com/kowainik/hit-on/workflows/CI/badge.svg)](https://github.com/kowainik/hit-on/actions)
[![Build status](https://img.shields.io/travis/kowainik/hit-on.svg?logo=travis)](https://travis-ci.org/kowainik/hit-on)

[![Hackage](https://img.shields.io/hackage/v/hit-on.svg?logo=haskell)](https://hackage.haskell.org/package/hit-on)
[![Stackage Lts](http://stackage.org/package/hit-on/badge/lts)](http://stackage.org/lts/package/hit-on)
[![Stackage Nightly](http://stackage.org/package/hit-on/badge/nightly)](http://stackage.org/nightly/package/hit-on)

[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)

**Kowainik Git Workflow Helper Tool.**

You can find the description of the workflow here:

* [Kowainik Git Workflow](https://github.com/kowainik/org#workflow)

`hit-on`  provides the `hit` binary with a convenient command-line interface to improve the interaction with [`git`][git] in a compatible way with the described working methods. It saves time for people who use this workflow on a daily basis, helps beginners expand their insight of the core VCS processes and makes collaboration between team members easier during development.

Here is an example of how you can see the list of issues and the issue description with `hit`:

![hit issue](https://user-images.githubusercontent.com/8126674/54478225-cae07c00-484a-11e9-86b3-c4c54e281274.png)

Or how you can see pretty short stats about your changes:

![hit status example](https://user-images.githubusercontent.com/4276606/60981099-428d6600-a368-11e9-97aa-656a3dd38efa.png)

## Getting started

### Prerequisites

To start using `hit` make sure that you have the following tools installed on your machine:

+ [ `git`][git] — `hit` is a wrapper around `git`
* [`hub`](https://github.com/github/hub) – to make PRs to GitHub directly.
+ **Optional:** `diff-highlight` — for pretty output of the `hit diff` command
  + Linux installation instructions
    ```shell
    cd /usr/share/doc/git/contrib/diff-highlight/
    sudo make
    sudo chmod +x diff-highlight
    sudo ln -s diff-highlight /usr/local/bin/diff-highlight
    ```
  + [macOS installation instructions](https://www.viget.com/articles/dress-up-your-git-diffs-with-word-level-highlights/)

### Installation

There are several methods to install the `hit` tool. You can choose the one that you are most comfortable with.

#### Download from releases

You can download the `hit` binary directly from the GitHub releases:

* [`hit` releases](https://github.com/kowainik/hit-on/releases)

After downloading, make it executable and copy it to a convenient location, for example:

```shell
chmod +x hit-linux
mv hit-linux ~/.local/bin/hit
```

#### Build from source

> **NOTE:** the project is written in Haskell, so you need to have one of the Haskell build tools installed. See this [blog post](https://kowainik.github.io/posts/2018-06-21-haskell-build-tools) for installation and usage instructions.

You need to follow these steps:

1. Clone the repository from GitHub

    ```shell
    git clone https://github.com/kowainik/hit-on.git
    ```
2. Step into the directory

    ```shell
    cd hit-on
    ```

3. Install the project with one of the build tools
  * [Cabal](https://www.haskell.org/cabal/users-guide/)
     ```shell
        cabal new-install hit-on
     ```
    **Note:** make sure you have `~/.cabal/bin` in your $PATH
  * [Stack](https://docs.haskellstack.org/en/stable/README/)
     ```shell
        stack install hit-on
     ```

4. Make sure that `hit` is installed:

    ```shell
    hit --version
    ```

#### macOS package manager

Currently, this method of installation is not supported. See [this issue](https://github.com/kowainik/hit-on/issues/41) for more details or if you want to help.

#### Ubuntu package manager

Currently, this method of installation is not supported. See [this issue](https://github.com/kowainik/hit-on/issues/42) for more details or if you want to help.

### Setting up

Follow the steps below to configure `hit` :

1. Enable autocompletion by calling the following command:
    ```shell
    source <(hit --bash-completion-script `which hit`)
    ```
    Add it your personal config file (like `~/.bashrc`) to enable automatically.
2. Specify your GitHub login in the global `.gitconfig`
```shell
git config --global user.login <your_login>
```
3. **This step is only required if you want to use `hit` with private repositories**.
    1. [Create OAuth token on GitHub.](https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/) The following scopes for the token should be specified:
    ![Screenshot from 2019-03-16 22-34-57](https://user-images.githubusercontent.com/4276606/54476778-30793c00-483c-11e9-9625-a4f6ced40820.png)

    2. Copy the generated token.
    3. Export token as an environment variable
        ```shell
        export GITHUB_TOKEN=<paste_generated_token_here>
        ```

## Commands

 | Command  | Description                                                                |
 |----------|----------------------------------------------------------------------------|
 | hop      | Switch to branch and sync it                                               |
 | fresh    | Rebase current branch on remote one                                        |
 | new      | Create new branch from the current one                                     |
 | stash    | Stash all local changes                                                    |
 | unstash  | Unstash previously stashed changes                                         |
 | commit   | Commit all local changes and prepend issue number                          |
 | uncommit | Reset to the previous commit saving the changes                            |
 | fix      | Fix requested changes to the last commit                                   |
 | amend    | Amend changes to the last commit and force push                            |
 | issue    | Show the information about the issue                                       |
 | push     | Push the current branch                                                    |
 | sync     | Sync local branch with its remote                                          |
 | resolve  | Switch to master, sync and delete the branch                               |
 | clear    | Remove all local changes permanently                                       |
 | current  | Show info about current branch and issue (if applicable)                   |
 | status   | Show current branch and beautiful stats with COMMIT_HASH (by default HEAD) |
 | diff     | Display beautiful diff with COMMIT_HASH (by default HEAD)                  |
 | clone    | Clone the repo. Use 'reponame' or 'username/reponame' formats              |
 | log      | Outputs the log of the current commit or COMMIT_HASH                       |

## Usage

The best way to demonstrate the power of the `hit` tool on a day-to-day basis with our workflow is to go through the entire workflow step by step, solving an ordinary problem of the typical [`git`][git] user.

> Here we assume that you work with `origin` remote with the main branch set to `master`.

### hit clone

If you don't have the repository locally, you need to clone it. With the `git`
tool you would need to specify the full URL which you can get from the
repository GitHub page.

```shell
git clone git@github.com:username/project-name.git
```

`hit` can simplify this process a bit. If you want to clone the project which is
under your GitHub username you can write:

```shell
hit clone my-project
```

If this is not your personal repository then you can use `clone` command in the
following way:

```shell
hit clone owner-name/project-name
```

### hit hop

When you want to start working on a new issue, you usually want to make sure you're using the latest version of your project. As a `git` user you may use the following commands:

```shell
git checkout master
git pull --rebase --prune
```

With `hit` you can just:

```shell
hit hop
```

### hit issue

Now you need to decide which issue you want to work on. You can use the `hit issue` command to see the full list of all open issues. After choosing the number of the issue, let's say 42, call `hit issue 42` to see the details of that issue.

### hit new

Start your work in a new branch. According to our workflow, branch names should have the following form:

```
<user_login>/<issue_number>-<short_issue_description>
```

With `git` you can create a branch using the following command:

```shell
git checkout -b my-login/42-short-desc
```

`hit` allows you to accomplish this task in an easier manner:

```shell
hit new 42
```

It uses the issue title to generate a short description.

### hit status

Before commiting your changes, you may want to inspect short stats about your
work. With `git` you usually call the following command:

```shell
git status
```

However, the same `hit` command produces better output:

```shell
hit status
```

![hit status example](https://user-images.githubusercontent.com/4276606/60981099-428d6600-a368-11e9-97aa-656a3dd38efa.png)

### hit diff

If you want to see detailed diff of your changes, use `hit diff` command. If you
have `diff-hightlight` installed then `hit diff` outputs much nicer diffs.

### hit commit

After finishing your work on that issue, you need to commit your changes. With `git` you would do the following:

```shell
git add .
git commit -m "[#42] Implement my feature

Resolves #42"
```

With `hit` you need only to specify the text of the commit to get the same result:

```shell
hit commit "Implement my feature"
```

or even simplier:

```shell
hit commit
```

And the commit name would be the title of the corresponding issue at GitHub (if
you are currently in the branch named as described above).

Note that you don't need to keep in mind the current issue number. However, if you want to refresh the context about the issue, use the `hit current` command.

### hit push

After committing your changes locally, you need to push them to the remote repository. It's usually a good practice to push only the current branch.

The `git` command for this is a little bit verbose:

```shell
git push -u origin my-login/42-short-desc
```

`hit` allows you to save several keystrokes:

```shell
hit push
```

> __Note:__ `hit push` command can be combined with the `hit commit` command
> using `-p|push` flag in the latter command.
> ```shell
> hit commit --push
> ```

### hit sync

After opening the pull request, some of the reviewers suggested changes that you applied as commits to the remote branch via GitHub interface. Now you need to sync your local branch with the remote one.

With `git` you can do the following:

```shell
git pull --rebase origin my-login/42-short-desc
```

However, with `hit` you can just:

```shell
hit sync
```

### hit fresh

While you were waiting for the second round of reviews, another pull request was merged to the `master` branch. Now you need to apply the new `master` changes to your local branch.

With `git` you can do the following:

```shell
git fetch origin master
git rebase origin/master
```

Again, with `hit` you can do better:

```shell
hit fresh
```

### hit fix

Now you need to make changes to your work locally according to the code review and push them to the remote repository.

`git` requires from you to do several steps to accomplish this simple task:

```shell
git add .
git commit -m "Fix after review"
git push origin my-login/42-short-desc
```

`hit` helps you with this as well:

```shell
hit fix
```

### hit amend

Oops, you've just realised that you have made a typo in your work! So you fixed the typo. But now you want to update the remote branch without creating a new unnecessary commit.

With `git` you can do the following:

```shell
git commit -a --amend --no-edit
git push origin my-login/42-short-desc --force
```

With `hit` you can simply:

```shell
hit amend
```

### hit resolve

Hooray, your PR just got merged! It's time to clean your local repository and start working on a new issue!

With `git` you would do the following:

```shell
git checkout master
git pull --rebase --prune
git branch -D my-login/42-short-desc
```

With `hit` you can finish your work faster:

```shell
hit resolve
```

### hit log

Hooray, your PR just got merged! It's time to clean your local repository and start working on a new issue!

With `git` you would do the following:

```shell
git log --oneline --decorate [COMMIT_HASH]
```

With `hit` you can finish your work faster:

```shell
hit log [COMMIT_HASH]
```

[git]: https://git-scm.com/

## Acknowledgement

Icons made by [Freepik](http://www.freepik.com) from
[www.flaticon.com](https://www.flaticon.com/) is licensed by
[CC 3.0 BY](http://creativecommons.org/licenses/by/3.0/).
