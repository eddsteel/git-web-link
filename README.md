# git-web-link

``` shell
$ git web-link help
Usage: git web-link [options] [parameters]

Provide a URL for the web-location of this file or dir in the given
remote repository's web UI. Repository providers currently supported: github
and github enterprise.

Example

$ git web-link origin app/Main.hs 31 32
https://github.com/eddsteel/git-web-link/blob/example/app/Main.hs#L31-L32

Options

(none)                         Uses the currently active branch.
-b <branch>                    Specify the branch to link to.
-d                             Deference branch name to canonical reference
Note: specifying -b after -d will squash it.

Parameters

(nome)                         Prints usage (this)
help                           Prints usage (this)
<remote>                       Links to the project root on the given branch and remote.
<remote> <file or dir path>    Links to the given blob/tree on the given branch and remote.
<remote> <file path> <line>    Links to a specific line in the given file/ branch/ remote.
<remote> <file> <start> <end>  Links to a range of lines in the given file/ branch/ remote.
```

## Completed Features

### Slice 1 — v0.1

Feature complete for github/github enterprise. If remote isn't
github.com it will assume github enterprise. It will assume dir/tree
with a trailing '/', file/blob otherwise.

### Slice 2 — v0.2

Paths relative from working dir are supported, so this is actually
useful to call from an editor. Some code renaming and clean up too.

There are still some issues (documented below).

### Slice 3 — v0.3

Strip `.git` from link if it's there in the remote.
Handle root dir on branch correctly for Github.
Don't provide a URL for a line number/range if a directory, not file, is given.

### Slice 4 — v0.4

Provide `-d'` flag to dereference -b argument (or active branch) to provide canonical URLs. E.g.

```
~/src/git-web-link $ git web-link -b master origin src/GitWebLink.hs
https://github.com/eddsteel/git-web-link/blob/master/src/GitWebLink.hs
~/src/git-web-link $ git web-link -b master -d origin src/GitWebLink.hs
https://github.com/eddsteel/git-web-link/blob/a19d542032be29a43bed3a1a50b5d70d773ac52c/src/GitWebLink.hs
~/src/git-web-link $ git web-link -d origin
https://github.com/eddsteel/git-web-link/tree/a19d542032be29a43bed3a1a50b5d70d773ac52c
```

### Slice 5 — v0.5

- Rewrite with options, not parameters.
- Make remote an option, with fallback to branch's active remote.
- Provide completion for branches, remotes and paths.

## Upcoming Features


### Slice 6 — v0.6

Provide `-t` to reference by tag (with `-d` support). Refactor to talk about Refs, not Branches.

### Unscheduled (PRs welcome)

- Add Gitlab.
- Add bitbucket.
- Add others. Branchable? Git web?
- Default to remote if only one is specified.

## Known Issues that will be tackled

- You can pass a directory and a line number/range, which doesn't make sense.
