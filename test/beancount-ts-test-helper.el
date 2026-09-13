;;; beancount-ts-test-helper.el --- Shared setup for the test files -*- lexical-binding: t; -*-

;;; Commentary:

;; Loaded by every test file: puts the package on the load path, finds
;; the tree-sitter grammar, and defines the one macro that runs a test
;; body in a mode buffer holding a sample ledger.

;;; Code:

(require 'ert)

(defconst beancount-ts-test--package-directory
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))
  "The directory holding the package sources.")

(add-to-list 'load-path beancount-ts-test--package-directory)
(require 'beancount-ts-mode)
(require 'beancount-ts-refile)

;; `make grammar' installs the pinned build under ~/.emacs.d/tree-sitter,
;; which `treesit-ready-p' searches on its own.  Only when no build is
;; found there fall back to the one Doom keeps in its cache dir, which
;; batch Emacs never sees because its `user-emacs-directory' is bare.
;; The order matters: `treesit-extra-load-path' is searched first, so
;; adding Doom's dir unconditionally would shadow a freshly pinned
;; build with whatever Doom last built.
(unless (treesit-ready-p 'beancount t)
  (let ((dir (expand-file-name "~/.emacs.d/.local/cache/tree-sitter")))
    (when (file-directory-p dir)
      (add-to-list 'treesit-extra-load-path dir))))

;; The tree-sitter tests skip themselves without a grammar, and ERT
;; exits 0 on skips, so a clone failure or a missing compiler on the
;; runner would turn most of the suite off without turning CI red.
(when (and (getenv "CI") (not (treesit-ready-p 'beancount t)))
  (error "The beancount grammar is not installed; the tree-sitter tests would all skip"))

(defmacro beancount-ts-test--in-buffer (ledger &rest body)
  "Run BODY in a `beancount-ts-mode' buffer holding LEDGER, point at its start.
Skips the test when the grammar is missing."
  (declare (indent 1))
  `(progn
     (skip-unless (treesit-ready-p 'beancount t))
     (with-temp-buffer
       (insert ,ledger)
       (beancount-ts-mode)
       (goto-char (point-min))
       ,@body)))

(provide 'beancount-ts-test-helper)
;;; beancount-ts-test-helper.el ends here
