# `vc-use-package`

Primitive integration of `package-vc.el` into [use-package].

## Installation

Note that, as of 2023-05-16, `vc-use-package` has been
[merged](https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=2ce279680bf9c1964e98e2aa48a03d6675c386fe)
into Emacs `master`!
Thus, if you are using a recent enough version,
you don't need to install this package and can instead use `use-package`s own `:vc` keyword.

Install with `package-vc-install`:

``` emacs-lisp
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
```

## Usage

This package adds a new `:vc` keywords to `use-package` declarations,
with which you can install packages.  For example:

``` emacs-lisp
(use-package math-delimiters
  :vc (:fetcher github :repo oantolin/math-delimiters))
```

The argument to `:vc` is a plist with the following keywords: `:fetcher
:repo :rev :backend`.

For a list of supported fetchers, see `vc-use-package-fetchers`.
Additionally, `:fetcher` may also be a URL, in which case it is followed
verbatim.  The `:rev` and `:backend` keywords are given to
`vc-package-install` as-is.  The `:fetcher` and `:repo` arguments may
either be strings, or symbols (as above).

### Directly using `package-vc.el`s interface

By giving `:vc` a cons-pair of a package name, as well as a relevant
plist, one can also directly use the interface specified by
`package-vc-selected-packages`.  This can be used, for example, to check
out specific branches:

``` emacs-lisp
(use-package modus-themes
  :vc (modus-themes :url "https://gitlab.com/protesilaos/modus-themes"
                    :branch "main"))
```

Another example would be to correctly install a package with an
"extension" directory:

``` emacs-lisp
(use-package dirvish
  :vc (dirvish :url "https://github.com/alexluigit/dirvish"
               :lisp-dir "extensions/"))
```

### In combination with `use-package-always-ensure`

When using `use-package-always-ensure`, make sure that the variable is
set at the point when the package is loaded; everything should work
correctly in that case.

If setting the variable at a later point is desirable, call
`vc-use-package-activate-advice` after doing so.  Additionally, the
`vc-use-package-deactivate-advice` function exists to remove this
behaviour.

[use-package]: https://github.com/jwiegley/use-package/
