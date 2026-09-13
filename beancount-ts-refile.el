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
  "What the last refile moved, for `beancount-ts-undo-last-refile'.
A plist: :source is the source buffer, :entries the (MARKER . TEXT)
pairs of what was cut from it, and :targets the extents appended to
the target buffers, as `beancount-ts--append-to-buffer' returns them.")

(defconst beancount-ts--refileable-entry-types
  '("transaction" "balance" "price")
  "Entry node types the refiler files by inference.
Transactions and balances resolve through their accounts, prices
through `beancount-ts-prices-file'.  Account lifecycle directives
\(open, close, commodity) are deliberately excluded: they belong to
accounts.beancount rather than to a per-account journal.")

(defun beancount-ts--refileable-p (node)
  "Return non-nil when NODE is an entry the refiler handles."
  (and (treesit-node-check node 'named)
       (member (treesit-node-type node) beancount-ts--refileable-entry-types)))

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

(defun beancount-ts--file-index (files)
  "Return FILES as the index the matchers take: (NORMALIZED . PATH) per file.
Built once per refile, so each journal file is normalised once rather
than on every lookup; inference looks a file up once per truncation
step per account per entry.  Order is preserved, and every matcher
returns the first hit in it.

  (beancount-ts--file-index \\='(\"liabilities/quicksilver-visa.beancount\"))
  => ((\"liabilities/quicksilvervisa.beancount\"
       . \"liabilities/quicksilver-visa.beancount\"))"
  (mapcar (lambda (file) (cons (beancount-ts--normalize-path-part file) file))
          files))

(defun beancount-ts--match-file (candidate index)
  "Return the file in INDEX equal to CANDIDATE, ignoring hyphenation.
Prefers an exact match; falls back to comparing normalized paths.
INDEX is from `beancount-ts--file-index'."
  (or (cdr (seq-find (lambda (entry) (string= candidate (cdr entry))) index))
      (cdr (assoc (beancount-ts--normalize-path-part candidate) index))))

(defun beancount-ts--find-best-file-match (guess index)
  "Find the best matching file in INDEX for path GUESS.
GUESS is like \"liabilities/capital-one/quicksilver-visa\"; INDEX is
from `beancount-ts--file-index'.  Used as a fuzzy fallback for prompt
defaults: the candidate sharing the longest run of leading path
components wins, first in INDEX on a tie."
  (or (beancount-ts--match-file (concat guess ".beancount") index)
      (let ((best nil)
            (best-score 0)
            (guess-parts (mapcar #'beancount-ts--normalize-path-part
                                 (split-string guess "/"))))
        (pcase-dolist (`(,normalized . ,cand) index)
          (let ((score 0)
                (g guess-parts)
                (c (split-string (string-remove-suffix ".beancount" normalized) "/")))
            (while (and g c (equal (car g) (car c)))
              (setq score (1+ score))
              (setq g (cdr g))
              (setq c (cdr c)))
            (when (> score best-score)
              (setq best cand)
              (setq best-score score))))
        best)))

(defun beancount-ts--find-ancestor-file (guess index)
  "Find the closest ancestor file for path GUESS in INDEX.
Try GUESS.beancount first, then progressively remove the last path
component.  Stop when fewer than 2 components remain.
Returns the matching relative path, or nil."
  (let ((parts (split-string guess "/"))
        (result nil))
    (while (and (not result) (>= (length parts) 2))
      (let ((match (beancount-ts--match-file
                    (concat (string-join parts "/") ".beancount")
                    index)))
        (if match
            (setq result match)
          (setq parts (butlast parts)))))
    result))

(defun beancount-ts--default-file-for (guess index)
  "Return the file in INDEX to offer as prompt default for path GUESS.
The closest ancestor file comes first, the same choice inference
makes, so a parent file is preferred to a sibling account's file that
merely shares a longer prefix.  Only when no ancestor exists does the
fuzzy prefix scorer get a say.

For example, with \"liabilities/chase.beancount\" and
\"liabilities/chase/sapphire.beancount\" on disk, the guess
\"liabilities/chase/ink-visa\" defaults to the former."
  (or (beancount-ts--find-ancestor-file guess index)
      (beancount-ts--find-best-file-match guess index)))

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

(defun beancount-ts--relevant-accounts (entry)
  "Get accounts from ENTRY suitable for refile target inference.
Accounts matching `beancount-ts-refile-ignored-account-prefixes' are
dropped."
  (seq-remove #'beancount-ts--ignored-account-p
              (beancount-ts--entry-accounts entry)))

(defun beancount-ts--primary-account (entry)
  "Return the account of ENTRY the prompt default is guessed from.
The first account inference would consider: the one configurable
ignore list governs both, so a journal with renamed roots or a
narrowed list gets a default too."
  (car (beancount-ts--relevant-accounts entry)))

(defun beancount-ts--infer-target-file (entry index)
  "Infer the target journal file for ENTRY among the files in INDEX.
A price directive names a commodity rather than an account, so it goes
to `beancount-ts-prices-file' when that file exists.  Otherwise examine
each relevant account, find the closest ancestor journal file, and
return the relative path if all matched accounts agree on one file.
Accounts that don't resolve to any file are ignored.
Return nil if ambiguous or no accounts resolve."
  (if (equal (treesit-node-type entry) "price")
      (cdr (seq-find (lambda (entry) (string= beancount-ts-prices-file (cdr entry)))
                     index))
    (let* ((accounts (beancount-ts--relevant-accounts entry))
           (matches (delq nil
                          (mapcar (lambda (acct)
                                    (beancount-ts--find-ancestor-file
                                     (beancount-ts--account-to-path-guess acct)
                                     index))
                                  accounts)))
           (unique (delete-dups (copy-sequence matches))))
      (when (= (length unique) 1)
        (car unique)))))

(defun beancount-ts--entry-at-point ()
  "Return the refileable entry node containing point, or nil."
  (beancount-ts--enclosing-node #'beancount-ts--refileable-p))

(defun beancount-ts--collect-entries-in-region (beg end)
  "Return the refileable entry nodes lying wholly between BEG and END."
  (seq-filter #'beancount-ts--refileable-p
              (beancount-ts-entries-in-region beg end)))

(defun beancount-ts--list-journal-files (journal-root)
  "List all .beancount files under JOURNAL-ROOT as relative paths."
  (mapcar (lambda (f) (file-relative-name f journal-root))
          (directory-files-recursively journal-root "\\.beancount\\'")))

(defun beancount-ts--delete-entry (start end)
  "Delete the entry spanning START to END from the current buffer.
Also consume one trailing blank line if present.  Return (MARKER
. TEXT): where the entry was, and exactly what was removed, so it can
be put back."
  (save-excursion
    (goto-char end)
    (when (looking-at-p "\n")
      (setq end (1+ end))))
  (let ((text (buffer-substring start end)))
    (delete-region start end)
    (cons (copy-marker start) text)))

(defun beancount-ts--append-to-buffer (target-buf texts)
  "Append entry TEXTS to TARGET-BUF, blank-line separated from what is there.
Return the extent (TARGET-BUF START END) of everything inserted, as
markers, so an undo can remove exactly that and nothing typed since.
Does not save the buffer; caller is responsible for saving."
  (with-current-buffer target-buf
    (goto-char (point-max))
    (let ((start (point-marker)))
      (unless (or (= (point) (point-min))
                  (looking-back "\n\n" (max (point-min) (- (point) 2))))
        (insert "\n"))
      (dolist (text texts)
        (insert text "\n"))
      (list target-buf start (point-marker)))))

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

(defun beancount-ts--undo-refile-targets (extents)
  "Remove the refiled text from the target buffers, EXTENTS by extent.
EXTENTS are (BUFFER START END) as `beancount-ts--append-to-buffer'
returns them.  Runs as an `apply' entry in the source's undo list, so
it records its own inverse there and a redo re-appends the text.
Refuses when a target buffer has been killed: restoring the source
while the file keeps the entries would duplicate them on disk."
  (dolist (extent extents)
    (unless (buffer-live-p (car extent))
      (user-error "Cannot undo the refile: the buffer for %s was killed"
                  (file-name-nondirectory
                   (or (buffer-file-name (car extent)) "a target")))))
  (let ((removed (mapcar (lambda (extent)
                           (pcase-let ((`(,buf ,start ,end) extent))
                             (with-current-buffer buf
                               (prog1 (list buf start (buffer-substring start end))
                                 (delete-region start end)))))
                         extents)))
    (push (list 'apply #'beancount-ts--redo-refile-targets removed)
          buffer-undo-list))
  (setq beancount-ts--last-refile nil))

(defun beancount-ts--redo-refile-targets (removed)
  "Put refiled text back into the target buffers after an undo.
REMOVED is what `beancount-ts--undo-refile-targets' took out: (BUFFER
MARKER TEXT) per target.  Records the inverse so the pair can be
undone and redone indefinitely."
  (let ((extents (mapcar (lambda (item)
                           (pcase-let ((`(,buf ,marker ,text) item))
                             (with-current-buffer buf
                               (goto-char marker)
                               (let ((start (point-marker)))
                                 (insert text)
                                 (list buf start (point-marker))))))
                         removed)))
    (push (list 'apply #'beancount-ts--undo-refile-targets extents)
          buffer-undo-list)))

(defun beancount-ts--group-by-target (pairs)
  "Group the texts of PAIRS by target, keeping both orders.
PAIRS are (TARGET . TEXT); the result is one (TARGET TEXT...) per
distinct target, targets in order of first appearance and texts in
their original order.

  (beancount-ts--group-by-target
   \\='((\"a.beancount\" . \"x\") (\"b.beancount\" . \"y\") (\"a.beancount\" . \"z\")))
  => ((\"a.beancount\" \"x\" \"z\") (\"b.beancount\" \"y\"))"
  (let (grouped)
    (pcase-dolist (`(,target . ,text) pairs)
      (if-let* ((cell (assoc target grouped)))
          (push text (cdr cell))
        (push (list target text) grouped)))
    (mapcar (lambda (cell) (cons (car cell) (nreverse (cdr cell))))
            (nreverse grouped))))

(defun beancount-ts--refile-entries (pairs root)
  "Move each (ENTRY . TARGET) in PAIRS from the current buffer into TARGET.
ENTRY is a node in this buffer, TARGET a path relative to ROOT, the
journal directory.  Every affected buffer gets one undo step, and
undoing in the source also undoes the targets.  Targets are saved
before the source so an interrupted refile fails towards a duplicate,
never a loss."
  (let* ((texts (mapcar (lambda (pair)
                          (cons (cdr pair) (treesit-node-text (car pair) t)))
                        pairs))
         ;; Positions before any edit: a node is not safe to ask once
         ;; the buffer changes under it.
         (ranges (mapcar (lambda (pair)
                          (cons (treesit-node-start (car pair))
                                (treesit-node-end (car pair))))
                        pairs))
         ;; One buffer per distinct target, in order of first appearance.
         (groups (mapcar (lambda (group)
                           (cons (find-file-noselect (expand-file-name (car group) root))
                                 (cdr group)))
                         (beancount-ts--group-by-target texts)))
         (target-bufs (mapcar #'car groups))
         (source-handle (prepare-change-group))
         (target-handles (mapcar #'prepare-change-group target-bufs))
         (removed nil)
         (extents nil))
    (activate-change-group source-handle)
    (dolist (h target-handles) (activate-change-group h))
    ;; An error between the first cut and the last append would leave
    ;; entries in no buffer; roll every buffer back instead.
    (let ((done nil))
      (unwind-protect
          (progn
            ;; Delete last first, so the earlier ranges keep their positions.
            (dolist (range (reverse ranges))
              (push (beancount-ts--delete-entry (car range) (cdr range)) removed))
            ;; Append grouped by target, each group in buffer order.
            (pcase-dolist (`(,buf . ,group) groups)
              (push (beancount-ts--append-to-buffer buf group) extents))
            (setq done t))
        (unless done
          (cancel-change-group source-handle)
          (dolist (h target-handles) (cancel-change-group h)))))
    (setq extents (nreverse extents))
    ;; Top of the source's undo group: undo the targets first.
    (push (list 'apply #'beancount-ts--undo-refile-targets extents)
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
          (list :source (current-buffer) :entries removed :targets extents))))

(defun beancount-ts--source-p (relative root)
  "Return non-nil when RELATIVE under ROOT is the file this buffer visits.
Both sides are resolved through symlinks: `find-file-visit-truename'
\(which Doom sets) makes `buffer-file-name' the real path while
`beancount-ts-journal-file' may reach the journal through a link, and
an entry already in its home file must be recognised as home either
way, or it is cut and re-appended to the same buffer."
  (and buffer-file-name
       (string= (file-truename buffer-file-name)
                (file-truename (expand-file-name relative root)))))

(defun beancount-ts--refile-single (entry)
  "Refile ENTRY, inferring its target or prompting when that is ambiguous."
  (let* ((root (beancount-ts--journal-root))
         (all-files (beancount-ts--list-journal-files root))
         (index (beancount-ts--file-index all-files))
         (inferred (beancount-ts--infer-target-file entry index))
         (target
          (if (and inferred (not (beancount-ts--source-p inferred root)))
              inferred
            ;; Ambiguous or same file: prompt, best guess as the default.
            (let* ((primary (beancount-ts--primary-account entry))
                   (default (or inferred
                                (and primary
                                     (beancount-ts--default-file-for
                                      (beancount-ts--account-to-path-guess primary)
                                      index)))))
              (completing-read "Refile entry to: " all-files nil t nil nil default)))))
    ;; `completing-read' answers "" to RET on an empty prompt whatever
    ;; REQUIRE-MATCH says; "" would expand to the journal root.
    (unless (member target all-files)
      (user-error "No target file chosen"))
    (when (beancount-ts--source-p target root)
      (user-error "Target file is the same as the source file"))
    (beancount-ts--refile-entries (list (cons entry target)) root)
    (message "Refiled entry to %s" target)))

(defun beancount-ts--refile-batch (entries)
  "Refile ENTRIES whose target can be inferred; leave the rest in place."
  (unless entries (user-error "No entries found"))
  (let* ((root (beancount-ts--journal-root))
         (index (beancount-ts--file-index (beancount-ts--list-journal-files root)))
         (ambiguous 0)
         pairs)
    (dolist (entry entries)
      (let ((target (beancount-ts--infer-target-file entry index)))
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
      (beancount-ts--refile-entries pairs root)
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
  "Put the entries of the last refile back where they were cut from.
The appended text leaves the targets and the entries return to their
places in the source, whatever has been edited since: this is not an
undo of the newest change but a reversal of that one refile.  A plain
`undo' in the source right after the refile does the same."
  (interactive)
  (unless beancount-ts--last-refile
    (user-error "No refile to undo"))
  (let ((source (plist-get beancount-ts--last-refile :source))
        (entries (plist-get beancount-ts--last-refile :entries))
        (targets (plist-get beancount-ts--last-refile :targets)))
    (unless (buffer-live-p source)
      (user-error "Cannot undo the refile: the source buffer was killed"))
    (with-current-buffer source
      (undo-boundary)
      (beancount-ts--undo-refile-targets targets)
      (pcase-dolist (`(,marker . ,text) entries)
        (goto-char marker)
        (insert text))
      (undo-boundary))
    (message "Undid last refile")))


(provide 'beancount-ts-refile)
;;; beancount-ts-refile.el ends here
