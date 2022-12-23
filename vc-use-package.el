;;; vc-use-package.el --- Primitive `package-vc' integration into `use-package' -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Steckerhalter
;;               2022  Tony Zorman <soliditsallgood@mailbox.org>
;;
;; Author: steckerhalter (quelpa-use-package: https://github.com/quelpa/quelpa-use-package/)
;;         Tony Zorman   (vc-use-package: https://github.com/slotThe/vc-use-package)
;; Keywords: convenience, use-package, package-vc
;; Version: 0.1
;; Package-Requires: ((emacs "29.0") (dash "2.19.1"))
;; Homepage: https://github.com/slotThe/vc-use-package

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A `:vc' handler for `use-package'.  Install with `package-vc-install':
;;
;;     (package-vc-install "https://github.com/slotThe/vc-use-package")
;;
;; Example usage:
;;
;;     (use-package math-delimiters
;;       :vc (:fetcher github :repo oantolin/math-delimiters))
;;
;; See the README for more details.

;;; Code:

(require 'dash)
(require 'use-package-ensure)

(defvar vc-use-package-keywords
  '(:fetcher :repo :rev :backend)
  "All arguments that `package-vc-install' supports.")

(defvar vc-use-package-fetchers
  '(:github "https://github.com/"
    :gitlab "https://gitlab.com/"
    :codeberg "https://codeberg.org/"
    :sourcehut "https://git.sr.ht/~")
  "Places from where to fetch packages.")

(cl-defun vc-use-package--install (&key fetcher repo name rev backend)
  "Thin wrapper around `package-vc-install'.
This exists so we can have sane keywords arguments, yet don't
have to go overboard when normalising."
  (package-vc-install (concat fetcher repo) name rev backend))

;;;; Normalisation

(defun vc-use-package--check-fetcher (val)
  "Check whether VAL is a correct `:fetcher' argument.
More specifically, check if it's (i) URL or (ii) either a string
or a symbol representing one possible destination in
`vc-use-package-keywords'."
  (cond
   ((string-prefix-p "https://" val) val)
   ((plist-get vc-use-package-fetchers (intern (concat ":" val))))
   (t (use-package-error
       (format ":fetcher is not a url or one of %s."
               (-filter #'keywordp vc-use-package-fetchers))))))

(defun vc-use-package--normalise-args (args)
  "Normalise the plist given to `:vc'."
  (cl-flet* ((mk-string (s)
               (if (stringp s) s (symbol-name s)))
             (normalise (arg val)
               (pcase arg
                 (:fetcher (vc-use-package--check-fetcher (mk-string val)))
                 (:rev (if (eq val :last-release) val (mk-string val)))
                 (:repo (mk-string val))
                 (_ val))))
    (apply #'-concat
           (cl-loop for (k v) on args by #'cddr
                    collect (list k (normalise k v))))))

(defun vc-use-package--handle-errors (arg)
  "Primitive error handling for the most common cases."
  (cl-flet ((err (s &rest os)
              (use-package-error (apply #'format s os))))
    (let* ((keywords (cl-loop for (k _) on arg by #'cddr
                              collect k))
           (unknown-kws (-difference keywords vc-use-package-keywords)))
      (cond
       (unknown-kws
        (err ":vc declaration contains unknown keywords: %s.  Known keywords are: %s"
             unknown-kws
             vc-use-package-keywords))
       ((not (-contains? keywords :fetcher))
        (err ":vc declaration must at least contain the `:fetcher' keyword"))
       ((not (plistp arg))
        (use-package-error "Argument given to :vc must be a plist."))))))

(defun use-package-normalize/:vc (name _keyword args)
  (unless (and args (listp (car args)))
    (use-package-error ":vc wants a plist as an argument."))
  (let ((arg (car args)))
    (vc-use-package--handle-errors arg)
    (vc-use-package--normalise-args (plist-put arg :name name))))

;;;; Handler

(defun use-package-handler/:vc (name-symbol _keyword args rest state)
  (let ((body (use-package-process-keywords name-symbol rest state)))
    ;; This happens at macro expansion time, not when the expanded code
    ;; is compiled or evaluated.
    (if args
        (use-package-concat
         `((unless (package-installed-p ',(plist-get args :name))
             (apply #'vc-use-package--install ',args)))
         body)
      body)))

;;;; Play nice with `use-package-always-ensure'

(defun vc-use-package--override-:ensure (func name-symbol keyword ensure rest state)
  (let ((ensure (unless (plist-member rest :vc)
                  ensure)))
    (funcall func name-symbol keyword ensure rest state)))

(defun vc-use-package-activate-advice ()
  (advice-add 'use-package-handler/:ensure :around
              #'vc-use-package--override-:ensure))

(defun vc-use-package-deactivate-advice ()
  (advice-remove 'use-package-handler/:ensure
                 #'vc-use-package--override-:ensure))

;;;; Activate

(defun vc-use-package-set-keyword ()
  "Insert `vc-use-package-keyword' into `use-package-keywords'.
More specifically, insert it after `:unless' so that we only run
if either `:if', `:when', `:unless' or `:requires' are satisfied."
  (unless (member :vc use-package-keywords)
    (-let (((b a) (-partition-after-item :unless use-package-keywords)))
      (setq use-package-keywords
            (-concat b (cons :vc a))))))

(vc-use-package-set-keyword) ; register keyword on require

(when use-package-always-ensure
  (vc-use-package-activate-advice))

(provide 'vc-use-package)
;;; vc-use-package.el ends here
