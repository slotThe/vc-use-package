# `vc-use-package`

Primitive integration of `package-vc.el` into [use-package].

> [!NOTE]
> As of 2023-05-16, `vc-use-package` has been
> [merged](https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=2ce279680bf9c1964e98e2aa48a03d6675c386fe)
> into Emacs `master`!
> Thus, if you are using a recent enough version of Emacs 30+,
> you don't need to install this package and can instead use `use-package`s own `:vc` keyword.
>
> **Note that the keyword syntax differs in the core differs from that used in this package!**
> Instead of a MELPA-style `:vc (:fetcher github :repo oantolin/math-delimiters)`, the built-in keyword
> uses ELPA-style `:vc (:url "https://github.com/oantolin/math-delimiters")` package specifications.
> Please refer to the use-package and the Emacs manual for more details.
> If you want to use the ELPA-style syntax with `vc-use-package`,
> see [here](#directly-using-package-vcels-interface).

## Installation

Install with `package-vc-install`:

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

### Directly using `package-vc.el`s interface

By giving `:vc` a cons-pair of a package name, as well as a relevant
plist, one can also directly use the interface specified by
`package-vc-selected-packages`.  For example, this can be used to check
out specific branches, or to exclusively use URLs for fetching packages:

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
set before `vc-use-package` is loaded; e.g.,

``` emacs-lisp
(setopt use-package-always-ensure t)
(require 'vc-use-package)
```

Everything should work out of the box in this case.

The function `vc-use-package-activate-advice` may also be called manually to install the necessary advice.
In the other direction, `vc-use-package-deactivate-advice` exists to remove this behaviour.

[use-package]: https://github.com/jwiegley/use-package/
