# git-web-link

``` shell
$ git-web-link --help
Usage: git-web-link [-r|--remote NAME] [-b|--branch NAME]
                    [-p|--path FILE-OR-DIR] [-l|--line NUM] [-m|--line-end NUM]
                    [-d|--deref] [-v|--version]

  Provides a URL for the web-location of this file or dir in the given
  remote repository’s web UI. Repository providers currently supported: github,
  bitbucket, gitlab, and github enterprise.

  Example

  $ git web-link -b example -r origin -p app/Main.hs -l 31 -m 32
  https://github.com/eddsteel/git-web-link/blob/example/app/Main.hs#L31-L32


Available options:
Available options:
  -r,--remote NAME         Link to this remote (defaults to branch default push)
  -b,--branch NAME         Link to this branch (defaults to active branch)
  -p,--path FILE-OR-DIR    Link to this file or directory
  -l,--line NUM            Link to this line number (or range starting here,
                           requires path option, set to a filename)
  -m,--line-end NUM        Link to range ending here (requires start and path
                           option, set to a filename)
  -d,--deref               Dereference to commit hash in link (off by default)
  -c,--commit HASH         Link to a commit, by hash (disregards path, branch,
                           line, region, etc)
  -v,--version             Show version information
  -h,--help                Show this help text
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

### Slice 6 — v0.6

- Add `-c` to link to a commit (not a path _at_ a commit, but the web provider's commit description page).

### Slice 7  — v0.7

- Add bitbucket and gitlab support.

## Upcoming Features

### Slice 8 — v0.8

Support reference by specific commit (`-c`), tag (`-t`) or branch (`-b`), with `-d` support.. Refactor to use Refs, not Branches.

### Unscheduled (PRs welcome)

- Add others. Branchable? Git web?
