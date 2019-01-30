# Emcas Lisp for Ruby Development Environment (Elrb)

Elrb is an Emacs package for Ruby development. (Currently, it's quite similar to [Elpy](https://github.com/jorgenschaefer/elpy))

## Current Features

- Highlight code block
- Find file in project

## Normal Usage

- Occur buffer of `class` `def` `module` definitions: `M-x elrb-occur-definitions RET` (C-c C-d)
- Find file in project (the directory which decide the place by your .git, .svn, .hg): `M-x elrb-find-file` (C-c C-f)
- Change the project root: `M-x elrb-set-project-root`

## TODO

- Jump to definitions
- Ruby unit test
- Rubocop check
- ...
