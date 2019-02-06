# hit-on

[![Hackage](https://img.shields.io/hackage/v/hit-on.svg)](https://hackage.haskell.org/package/hit-on)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/hit-on/badge/lts)](http://stackage.org/lts/package/hit-on)
[![Stackage Nightly](http://stackage.org/package/hit-on/badge/nightly)](http://stackage.org/nightly/package/hit-on)
[![Build status](https://secure.travis-ci.org/kowainik/hit-on.svg)](https://travis-ci.org/kowainik/hit-on)

**Kowainik Git Workflow Helper Tool.**

You can find the description of the workflow here:

* [Kowainik Git Workflow](https://github.com/kowainik/org#workflow)

`hit-on`  provides the `hit` binary with the convenient command-line interface to improve the interaction with [`git`][git] in a compatible way with the described working methods. It saves time for the people who use this workflow on a daily basis and helps beginners to expand the insight of the core VCS processes and ease the cooperation work with the team during development.

## Getting started

### Prerequisites

To start using `hit` make sure that you have the following tools installed on your machine:

* [ `git`][git] — `hit` is a wrapper around `git`

### Installation

There are several methods to install `hit` tool. You can choose the one that you are most comfortable with.

#### Download from releases

You can download the `hit` binary directly from the GitHub releases:

* [`hit` releases](https://github.com/kowainik/hit-on/releases)

After downloading, make it executable and copy it under a convenient location, for example:

```shell
chmod +x hit-linux
mv hit-linux ~/.local/bin/hit
```

#### From the sources

> **NOTE:** the project is written in Haskell, so you need to have one of the Haskell build tools installed. See this [blog post]() for installation and usage instructions.

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
  * [Cabal]()
     ```shell
        cabal new-install hit
     ``` 
    **Note:** make sure you have `~/.cabal/bin` in the $PATH
  * [Stack]()
     ```shell
        stack install hit
     ``` 
4. Make sure the `hit` is installed:

```shell
hit --version
```
#### macOS package manager

Currently, this method of installation is not supported. See [this issue]() for more details or if you want to help.

#### Ubuntu package manager

Currently, this method of installation is not supported. See [this issue]() for more details or if you want to help.

### Setting up

Follow the steps below to configure `hit` :

1. Specify your GitHub login in the global `.gitconfig`
```shell
git config --global user.login <your_login>
```
2. **This step is only required if you want to use `hit` with private repositories**. 
    2.1. [Create OAuth token on GitHub.](https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/)
    2.2. Copy the generated token.
    2.3. Export token as the environment variable
        ```shell
        export GITHUB_TOKEN=<paste_generated_token_here>
        ```

## Usage

The best way to demonstrate the power of the `hit` tool on the day-to-day basis with our workflow is to go through the entire workflow step by step solving an ordinary problem of the typical `git` user.

> Here we assume that you work with `origin` remote with the main branch set to `master`.

### hit hop

When you want to start working on the new issue, make sure you're using the latest version of your project. As a `git` user you may use the following commands:

```shell
git checkout master
git pull --rebase --prune
```

With `hit` you can just:

```shell
hit hop
```

### hit issue

Now you need to decide which issue you should work at. You can use the `hit issue` command to see the full list of all open issues. After choosing the number `n` of the issue you want to resolve, call `hit issue n` to see the details of that issue.

### hit new

Start your work in a new branch. According to our workflow, branch names should have the following form:

```
<user_login>/<issue_number>-<short_issue_description>
```

With `git` you can create a branch using the following command:

```shell
git checkout -b my-login/42-short-desc
```

`hit` allows to accomplish this task easier:

```shell
hit new 42
```

It uses the issue title to generate a short description.

### hit commit

After finishing your work on that issue, you need to commit your changes. With `git` you would do the following:

```shell
git add .
git commit -m "[#42] Implement my feature

Resolves #42
```

With `hit` you need only to specify the text of the commit to get the same result:

```shell
hit commit "Implement my feature"
```

With `hit` you don't need to keep in mind the current issue number. However, if you want to refresh the context about the issue, use `hit current` command.

### hit push

After committing your changes locally, you need to push them to the remote repository. It's usually a good practice only to push the current branch. 

`git` command for this is a little bit verbose:

```shell
git push -u origin my-login/42-short-desc
```

`hit` allows you to save several keystrokes:

```shell
hit push
``` 

### hit sync

After opening the pull request, some of the reviewers suggested changes that you applied as commits to the remote branch. Now you need to sync your local branch with the remote one. 

With `git` you can do the following:

```shell
git pull --rebase origin my-login/42-short-desc
```

However, with `hit` you can just:

```shell
hit sync
```

### hit fresh

While you were waiting for the second review round, another pull request was merged to the `master` branch. Now you need to apply the new `master` changes to your local branch.

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

Oops, you've just realised that you have made a typo in your work! Now you want to fix this problem but without creating new unnecessary commit. 

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

[git]: https://git-scm.com/
