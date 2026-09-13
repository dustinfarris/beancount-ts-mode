;;; beancount-ts-refile.el --- Refile Beancount entries by account -*- lexical-binding: t -*-

;; Copyright (C) 2026 Dustin Farris
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Dustin Farris <dustin.farris@gmail.com>
;; Keywords: languages, beancount, tree-sitter

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Moves entries from an inbox ledger into per-account journal files,
;; inferring the target from the accounts an entry names.

;;; Code:

(require 'beancount-ts-mode)
(require 'seq)
(require 'subr-x)

;;; Refiling

(defvar beancount-ts--last-refile nil
  "Info about the last refile for multi-buffer undo.
Plist with :source SOURCE-BUFFER and :targets (TARGET-BUFFER ...).")

(defconst beancount-ts--refileable-entry-types
  '("transaction" "balance" "price")
  "Entry node types the refiler files by inference.
Transactions and balances resolve through their accounts, prices
through `beancount-ts-prices-file'.  Account lifecycle directives
\(open, close, commodity) are deliberately excluded: they belong to
accounts.beancount rather than to a per-account journal.")

(defcustom beancount-ts-prices-file "prices.beancount"
  "Journal-relative file that price directives refile into.
Price directives name a commodity rather than an account, so they
cannot be inferred the way transactions and balances are."
  :type 'string
  :group 'beancount-ts)

(defun beancount-ts--journal-root ()
  "Return the journal directory, taken from `beancount-ts-journal-file'.
The same setting points the language server at the journal, so the
two cannot disagree."
  (unless beancount-ts-journal-file
    (user-error "Set `beancount-ts-journal-file' to refile"))
  (file-name-directory (expand-file-name beancount-ts-journal-file)))

(defun beancount-ts--camel-to-kebab (s)
  "Convert CamelCase string S to kebab-case."
  (let* ((case-fold-search nil)
         (s1 (replace-regexp-in-string "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1-\\2" s))
         (s2 (replace-regexp-in-string "\\([A-Z]+\\)\\([A-Z][a-z]\\)" "\\1-\\2" s1)))
    (downcase s2)))

(defcustom beancount-ts-refile-entities nil
  "Account components in slot two that name a business entity.
An entity is hoisted to the leading path component, so with
\"Consulting\" listed here \"Liabilities:Consulting:Chase:InkVisa\"
resolves under \"consulting/liabilities/chase/\" rather than
\"liabilities/consulting/chase/\"."
  :type '(repeat string)
  :group 'beancount-ts)

(defcustom beancount-ts-refile-ignored-account-prefixes
  '("Expenses:" "Equity:")
  "Account prefixes that never decide where an entry is refiled.
An entry is filed by the accounts it names; the ones matching a prefix
here are left out of that inference, and out of the prompt default."
  :type '(repeat string)
  :group 'beancount-ts)

(defun beancount-ts--ignored-account-p (account)
  "Return non-nil when ACCOUNT starts with an ignored prefix."
  (seq-some (lambda (prefix) (string-prefix-p prefix account))
            beancount-ts-refile-ignored-account-prefixes))

(defun beancount-ts--account-to-path-guess (account)
  "Derive a best-guess journal-relative file path from ACCOUNT.
ACCOUNT is like \"Liabilities:CapitalOne:QuicksilverVisa\".
Returns a string like \"liabilities/capitalone/quicksilver-visa\".
A second component listed in `beancount-ts-refile-entities' leads."
  (let* ((parts (split-string account ":"))
         (account-type (car parts))
         (rest (cdr parts))
         path-parts)
    (if (and rest (member (car rest) beancount-ts-refile-entities))
        (let ((entity (car rest))
              (remaining (cdr rest)))
          (setq path-parts
                (append (list (beancount-ts--camel-to-kebab entity)
                              (beancount-ts--camel-to-kebab account-type))
                        (mapcar #'beancount-ts--camel-to-kebab remaining))))
      (setq path-parts
            (cons (beancount-ts--camel-to-kebab account-type)
                  (mapcar #'beancount-ts--camel-to-kebab rest))))
    (string-join path-parts "/")))

(defun beancount-ts--normalize-path-part (part)
  "Normalize path component PART for comparison.
Journal directories spell some multi-word account components solid
\(`capitalone', `mastercard') and others hyphenated
\(`quicksilver-visa'), so `beancount-ts--camel-to-kebab' cannot match
both by rule.  Dropping hyphens lets either spelling resolve."
  (replace-regexp-in-string "-" "" (downcase part)))

(defun beancount-ts--match-file (candidate all-files)
  "Return the entry of ALL-FILES equal to CANDIDATE, ignoring hyphenation.
Prefers an exact match; falls back to comparing normalized paths."
  (or (car (member candidate all-files))
      (let ((norm (beancount-ts--normalize-path-part candidate)))
        (seq-find (lambda (f)
                    (string= norm (beancount-ts--normalize-path-part f)))
                  all-files))))

(defun beancount-ts--find-best-file-match (guess candidates)
  "Find the best matching file from CANDIDATES for path GUESS.
GUESS is like \"liabilities/capital-one/quicksilver-visa\".  CANDIDATES
are relative paths, like
\"liabilities/capitalone/quicksilver-visa.beancount\".
Used as a fuzzy fallback for prompt defaults."
  (or (beancount-ts--match-file (concat guess ".beancount") candidates)
      (let ((best nil)
            (best-score 0)
            (guess-parts (mapcar #'beancount-ts--normalize-path-part
                                 (split-string guess "/"))))
        (dolist (cand candidates)
          (let* ((cand-parts (mapcar #'beancount-ts--normalize-path-part
                                     (split-string
                                      (string-remove-suffix ".beancount" cand) "/")))
                 (score 0)
                 (g guess-parts)
                 (c cand-parts))
            (while (and g c (equal (car g) (car c)))
              (setq score (1+ score))
              (setq g (cdr g))
              (setq c (cdr c)))
            (when (> score best-score)
              (setq best cand)
              (setq best-score score))))
        best)))

(defun beancount-ts--find-ancestor-file (guess all-files)
  "Find the closest ancestor file for path GUESS in ALL-FILES.
Try GUESS.beancount first, then progressively remove the last path
component.  Stop when fewer than 2 components remain.
Returns the matching relative path, or nil."
  (let ((parts (split-string guess "/"))
        (result nil))
    (while (and (not result) (>= (length parts) 2))
      (let ((match (beancount-ts--match-file
                    (concat (string-join parts "/") ".beancount")
                    all-files)))
        (if match
            (setq result match)
          (setq parts (butlast parts)))))
    result))

(defun beancount-ts--entry-accounts (entry)
  "Return every account named by ENTRY, in document order.
A transaction carries its accounts on postings; a balance directive
carries one directly as its `account' field.  Other entry kinds name
no account."
  (if (equal (treesit-node-type entry) "balance")
      (when-let* ((account (treesit-node-child-by-field-name entry "account")))
        (list (treesit-node-text account t)))
    (let (accounts)
      (dolist (child (treesit-node-children entry))
        (when (equal (treesit-node-type child) "posting")
          (dolist (pchild (treesit-node-children child))
            (when (equal (treesit-node-type pchild) "account")
              (push (treesit-node-text pchild t) accounts)))))
      (nreverse accounts))))

(defun beancount-ts--primary-account (entry)
  "Extract the primary account from ENTRY, for the prompt default.
Return the first Assets:, Liabilities:, or Income: account that
`beancount-ts-refile-ignored-account-prefixes' does not exclude."
  (seq-find (lambda (text)
              (and (not (beancount-ts--ignored-account-p text))
                   (or (string-prefix-p "Assets:" text)
                       (string-prefix-p "Liabilities:" text)
                       (string-prefix-p "Income:" text))))
            (beancount-ts--entry-accounts entry)))

(defun beancount-ts--relevant-accounts (entry)
  "Get accounts from ENTRY suitable for refile target inference.
Accounts matching `beancount-ts-refile-ignored-account-prefixes' are
dropped."
  (seq-remove #'beancount-ts--ignored-account-p
              (beancount-ts--entry-accounts entry)))

(defun beancount-ts--infer-target-file (entry all-files)
  "Infer the target journal file for ENTRY.
A price directive names a commodity rather than an account, so it goes
to `beancount-ts-prices-file' when that file exists.  Otherwise examine
each relevant account, find the closest ancestor journal file, and
return the relative path if all matched accounts agree on one file.
Accounts that don't resolve to any file are ignored.
Return nil if ambiguous or no accounts resolve."
  (if (equal (treesit-node-type entry) "price")
      (car (member beancount-ts-prices-file all-files))
    (let* ((accounts (beancount-ts--relevant-accounts entry))
           (matches (delq nil
                          (mapcar (lambda (acct)
                                    (beancount-ts--find-ancestor-file
                                     (beancount-ts--account-to-path-guess acct)
                                     all-files))
                                  accounts)))
           (unique (delete-dups (copy-sequence matches))))
      (when (= (length unique) 1)
        (car unique)))))

(defun beancount-ts--entry-at-point ()
  "Return the refileable entry node containing point, or nil."
  (treesit-parent-until
   (treesit-node-at (point))
   (lambda (n) (and (treesit-node-check n 'named)
                    (member (treesit-node-type n)
                            beancount-ts--refileable-entry-types)))))

(defun beancount-ts--collect-entries-in-region (beg end)
  "Return the refileable entry nodes lying wholly between BEG and END."
  (seq-filter (lambda (n) (member (treesit-node-type n)
                                  beancount-ts--refileable-entry-types))
              (beancount-ts-entries-in-region beg end)))

(defun beancount-ts--list-journal-files (journal-root)
  "List all .beancount files under JOURNAL-ROOT as relative paths."
  (mapcar (lambda (f) (file-relative-name f journal-root))
          (directory-files-recursively journal-root "\\.beancount\\'")))

(defun beancount-ts--delete-entry (entry)
  "Delete ENTRY from the current buffer.
Also consume one trailing blank line if present."
  (let ((start (treesit-node-start entry))
        (end (treesit-node-end entry)))
    (save-excursion
      (goto-char end)
      (when (looking-at-p "\n")
        (setq end (1+ end))))
    (delete-region start end)))

(defun beancount-ts--append-to-file (target-path texts)
  "Append entry TEXTS to the file at TARGET-PATH.
Ensure a blank line separator before the first inserted entry.
Does not save the buffer; caller is responsible for saving."
  (let ((target-buf (find-file-noselect target-path)))
    (with-current-buffer target-buf
      (goto-char (point-max))
      (unless (or (= (point) (point-min))
                  (looking-back "\n\n" (max (point-min) (- (point) 2))))
        (insert "\n"))
      (dolist (text texts)
        (insert text "\n")))))

(defun beancount-ts--save-buffers (buffers)
  "Save BUFFERS, returning an alist of (BUFFER . ERROR) for those that failed.
One buffer's failure must not abort the rest.  A save can fail for
reasons well outside the refiler -- a dead LSP server signalling from
`after-save-hook', a read-only file -- and by the time targets are
written the entries have already been cut from the source buffer, so
every target has to get its chance to reach disk."
  (let (failures)
    (dolist (buf buffers)
      (when (buffer-live-p buf)
        (condition-case err
            (with-current-buffer buf (save-buffer))
          (error (push (cons buf err) failures)))))
    (nreverse failures)))

(defun beancount-ts--commit-refile (source-buf target-bufs)
  "Write TARGET-BUFS then SOURCE-BUF to disk, or abort without touching source.
Targets are saved first on purpose: the source is the only copy of the
entries until a target reaches disk, so an interrupted refile must fail
towards a harmless duplicate rather than towards a loss."
  (let ((failures (beancount-ts--save-buffers target-bufs)))
    (when failures
      (user-error
       "Refile aborted: could not save %s. %s is unchanged on disk; undo to restore it"
       (mapconcat (lambda (f) (buffer-name (car f))) failures ", ")
       (buffer-name source-buf)))
    (with-current-buffer source-buf (save-buffer))))

(defun beancount-ts--undo-refile-targets (target-bufs)
  "Undo refile appends in TARGET-BUFS.
Called automatically via `buffer-undo-list' during undo."
  (dolist (buf target-bufs)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (undo))))
  (setq beancount-ts--last-refile nil))

(defun beancount-ts--refile-entries (pairs)
  "Move each (ENTRY . TARGET) in PAIRS from the current buffer into TARGET.
ENTRY is a node in this buffer, TARGET a journal-relative path.
Every affected buffer gets one undo step, and undoing in the source
also undoes the targets.  Targets are saved before the source so an
interrupted refile fails towards a duplicate, never a loss."
  (let* ((root (beancount-ts--journal-root))
         (texts (mapcar (lambda (pair)
                          (cons (cdr pair) (treesit-node-text (car pair) t)))
                        pairs))
         (target-bufs (delete-dups
                       (mapcar (lambda (pair)
                                 (find-file-noselect (expand-file-name (cdr pair) root)))
                               pairs)))
         (source-handle (prepare-change-group))
         (target-handles (mapcar #'prepare-change-group target-bufs)))
    (activate-change-group source-handle)
    (dolist (h target-handles) (activate-change-group h))
    ;; Delete last first, so the earlier nodes keep their positions.
    (dolist (pair (reverse pairs))
      (beancount-ts--delete-entry (car pair)))
    ;; Append grouped by target, each group in buffer order.
    (let (grouped)
      (pcase-dolist (`(,target . ,text) texts)
        (if-let* ((cell (assoc target grouped)))
            (setcdr cell (append (cdr cell) (list text)))
          (push (list target text) grouped)))
      (pcase-dolist (`(,target . ,group) (nreverse grouped))
        (beancount-ts--append-to-file (expand-file-name target root) group)))
    ;; Top of the source's undo group: undo the targets first.
    (push (list 'apply #'beancount-ts--undo-refile-targets target-bufs)
          buffer-undo-list)
    (accept-change-group source-handle)
    (undo-amalgamate-change-group source-handle)
    (dolist (h target-handles)
      (accept-change-group h)
      (undo-amalgamate-change-group h))
    ;; Close the step in every buffer, the source included: the command
    ;; loop would do it for the source, a Lisp caller would not.
    (undo-boundary)
    (dolist (buf target-bufs)
      (with-current-buffer buf (undo-boundary)))
    (beancount-ts--commit-refile (current-buffer) target-bufs)
    (setq beancount-ts--last-refile
          (list :source (current-buffer) :targets target-bufs))))

(defun beancount-ts--source-p (relative root)
  "Return non-nil when RELATIVE under ROOT is the file this buffer visits."
  (and buffer-file-name
       (string= (expand-file-name buffer-file-name)
                (expand-file-name relative root))))

(defun beancount-ts--refile-single (entry)
  "Refile ENTRY, inferring its target or prompting when that is ambiguous."
  (let* ((root (beancount-ts--journal-root))
         (all-files (beancount-ts--list-journal-files root))
         (inferred (beancount-ts--infer-target-file entry all-files))
         (target
          (if (and inferred (not (beancount-ts--source-p inferred root)))
              inferred
            ;; Ambiguous or same file: prompt, best guess as the default.
            (let* ((primary (beancount-ts--primary-account entry))
                   (default (or inferred
                                (and primary
                                     (beancount-ts--find-best-file-match
                                      (beancount-ts--account-to-path-guess primary)
                                      all-files)))))
              (completing-read "Refile entry to: " all-files nil t nil nil default)))))
    (when (beancount-ts--source-p target root)
      (user-error "Target file is the same as the source file"))
    (beancount-ts--refile-entries (list (cons entry target)))
    (message "Refiled entry to %s" target)))

(defun beancount-ts--refile-batch (entries)
  "Refile ENTRIES whose target can be inferred; leave the rest in place."
  (unless entries (user-error "No entries found"))
  (let* ((root (beancount-ts--journal-root))
         (all-files (beancount-ts--list-journal-files root))
         (ambiguous 0)
         pairs)
    (dolist (entry entries)
      (let ((target (beancount-ts--infer-target-file entry all-files)))
        (cond ((null target) (setq ambiguous (1+ ambiguous)))
              ((beancount-ts--source-p target root)) ; already home
              (t (push (cons entry target) pairs)))))
    (setq pairs (nreverse pairs))
    (cond
     ((and (null pairs) (> ambiguous 0))
      (message "No entries could be automatically refiled (%d ambiguous)" ambiguous))
     ((null pairs)
      (message "No entries to refile"))
     (t
      (beancount-ts--refile-entries pairs)
      (let ((n (length pairs)))
        (if (> ambiguous 0)
            (message "Refiled %d entr%s; %d ambiguous entr%s could not be refiled"
                     n (if (= n 1) "y" "ies")
                     ambiguous (if (= ambiguous 1) "y" "ies"))
          (message "Refiled %d entr%s" n (if (= n 1) "y" "ies"))))))))

;;;###autoload
(defun beancount-ts-refile-transaction ()
  "Refile entries to another beancount file.
Transactions, balance directives, and price directives are refileable.
If a region is active, auto-refile each entry individually, skipping
ambiguous ones.  Otherwise, refile the entry at point (auto if target
is clear, prompt if ambiguous)."
  (interactive)
  (if (use-region-p)
      (beancount-ts--refile-batch
       (beancount-ts--collect-entries-in-region
        (region-beginning) (region-end)))
    (let ((entry (beancount-ts--entry-at-point)))
      (unless entry (user-error "Not in a refileable entry"))
      (beancount-ts--refile-single entry))))

;;;###autoload
(defun beancount-ts-refile-buffer ()
  "Refile every entry in the buffer to its inferred journal file.
Transactions and balance directives are filed by their accounts, price
directives to `beancount-ts-prices-file'.  Ambiguous entries are left
in place with a summary message."
  (interactive)
  (beancount-ts--refile-batch
   (beancount-ts--collect-entries-in-region
    (point-min) (point-max))))

;;;###autoload
(defun beancount-ts-undo-last-refile ()
  "Undo the last refile across all affected buffers.
Reverts the source and target buffers to their pre-refile state."
  (interactive)
  (unless beancount-ts--last-refile
    (user-error "No refile to undo"))
  (let ((source (plist-get beancount-ts--last-refile :source))
        (targets (plist-get beancount-ts--last-refile :targets)))
    ;; Undo in target buffers first (remove appended transactions)
    (dolist (buf targets)
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (undo))))
    ;; Undo in source buffer (restore deleted transactions)
    (when (buffer-live-p source)
      (with-current-buffer source
        (undo)))
    (setq beancount-ts--last-refile nil)
    (message "Undid last refile")))


(provide 'beancount-ts-refile)
;;; beancount-ts-refile.el ends here
