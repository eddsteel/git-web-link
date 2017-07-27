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

Parameters

(nome)                         Prints usage (this)
help                           Prints usage (this)
<remote>                       Links to the project root on the given branch and remote.
<remote> <file or dir path>    Links to the given blob/tree on the given branch and remote.
<remote> <file path> <line>    Links to a specific line in the given file/ branch/ remote.
<remote> <file> <start> <end>  Links to a range of lines in the given file/ branch/ remote.
```

## Completed Features

### Slice 1 - v0.1

Feature complete for github/github enterprise. If remote isn't
github.com it will assume github enterprise. It will assume dir/tree
with a trailing '/', file/blob otherwise.

## Upcoming Features

### Unscheduled (PRs welcome)

- Check this supports commit hashes/ tags/ other ways of specifying a
  commit. `-b` may not be entirely appropriate.
- Add bitbucket.
- Add others. Branchable? Gitlab? Git web?
- Normalise file paths -- run with a relative path deep in the project
  it should work out the path relative to the project root.

## Known Issues

- You can pass a directory and a line number/range, which doesn't make sense.
