# `vc-use-package`

Primitive integration of `package-vc.el` into [use-package].

## Installation

Install with `package-vc-install`:

``` emacs-lisp
(package-vc-install "https://github.com/slotThe/vc-use-package")
```

After that, simply require the package somewhere in your configuration:

``` emacs-lisp
(require 'vc-use-package)
```

More comprehensively:

``` emacs-lisp
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)
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

By giving `:vc` a cons-pair of a package name, as well as a relevant
plist, one can also directly use the interface specified by
`package-vc-selected-packages`.  This can be used, for example, to check
out specific branches:

``` emacs-lisp
(use-package modus-themes
  :vc (modus-themes :url "https://gitlab.com/protesilaos/modus-themes"
                    :branch "main"))
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
