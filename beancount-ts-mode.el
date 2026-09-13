;;; beancount-ts-mode.el --- Tree-sitter major mode for Beancount -*- lexical-binding: t -*-

;; Copyright (C) 2026 Dustin Farris
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Author: Dustin Farris <dustin.farris@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.1"))
;; URL: https://github.com/dustinfarris/beancount-ts-mode
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

;; A major mode for editing Beancount files using tree-sitter for
;; syntax highlighting, indentation and structural navigation, with
;; commands to walk, clear, clone and sort transactions and an eglot
;; entry for beancount-language-server.  `beancount-ts-refile' adds
;; refiling of entries into per-account journal files.
;;
;; Built on Emacs's own `treesit' (Emacs 31+), and shaped like the stock
;; `*-ts-mode's: font-lock rules with a capture function for accounts,
;; `treesit-thing-settings' for entry and posting motion, imenu and
;; outline from the tree, and `derived-mode-add-parents' so packages
;; keyed on `beancount-mode' see this mode as one.

;;; Code:

(require 'treesit)
(require 'seq)
(require 'cl-lib)

(add-to-list 'treesit-language-source-alist
             '(beancount "https://github.com/polarmutex/tree-sitter-beancount" "v2.5.1"))

(defgroup beancount-ts nil
  "Support for Beancount using tree-sitter."
  :group 'languages
  :prefix "beancount-ts-")

(defcustom beancount-ts-journal-file nil
  "Path to the main beancount journal file.
Handed to beancount-language-server as its `journal_file'."
  :type '(choice (const nil) file)
  :group 'beancount-ts)

(defcustom beancount-ts-diagnostic-flags '("!")
  "Transaction flags beancount-language-server warns on.
Handed to the server as its `diagnostic_flags', which its own
flagged-entry scan honours.  The default mirrors the server's."
  :type '(repeat string)
  :group 'beancount-ts)

(defcustom beancount-ts-indent-offset 2
  "Number of columns to indent postings within a transaction."
  :type 'integer
  :group 'beancount-ts)

;;; Faces

(defface beancount-ts-date
  '((t :inherit font-lock-number-face))
  "Face for dates in beancount files."
  :group 'beancount-ts)

(defface beancount-ts-account
  '((t :inherit font-lock-builtin-face))
  "Face for root account component in beancount files."
  :group 'beancount-ts)

(defface beancount-ts-account-sub
  '((t :inherit default))
  "Face for sub-account components in beancount files."
  :group 'beancount-ts)

(defface beancount-ts-account-separator
  '((t :inherit font-lock-comment-face))
  "Face for colon separators in account names."
  :group 'beancount-ts)

(defface beancount-ts-amount
  '((t :inherit font-lock-number-face))
  "Face for amounts/numbers in beancount files."
  :group 'beancount-ts)

(defface beancount-ts-currency
  '((t :inherit font-lock-type-face))
  "Face for currencies in beancount files."
  :group 'beancount-ts)

(defface beancount-ts-directive
  '((t :inherit font-lock-keyword-face))
  "Face for directive keywords in beancount files."
  :group 'beancount-ts)

(defface beancount-ts-tag
  '((t :inherit font-lock-preprocessor-face))
  "Face for tags in beancount files."
  :group 'beancount-ts)

(defface beancount-ts-link
  '((t :inherit font-lock-preprocessor-face))
  "Face for links in beancount files."
  :group 'beancount-ts)

(defface beancount-ts-string
  '((t :inherit font-lock-string-face))
  "Face for strings/narrations in beancount files."
  :group 'beancount-ts)

(defface beancount-ts-metadata-key
  '((t :inherit font-lock-property-name-face))
  "Face for metadata keys in beancount files."
  :group 'beancount-ts)

(defface beancount-ts-flag-pending
  '((t :inherit font-lock-warning-face))
  "Face for pending transaction flag (!) in beancount files."
  :group 'beancount-ts)

(defface beancount-ts-flag-cleared
  '((t :inherit success))
  "Face for cleared transaction flag (*) in beancount files."
  :group 'beancount-ts)

;;; Font-lock

(defun beancount-ts--fontify-account (node override start end &rest _)
  "Fontify the account NODE one component at a time.
The root component gets `beancount-ts-account', later components
`beancount-ts-account-sub', and each colon
`beancount-ts-account-separator'.  OVERRIDE, START and END are the
capture-function arguments `treesit-font-lock-rules' passes."
  (let ((pos (treesit-node-start node))
        (node-end (treesit-node-end node))
        (root t))
    (dolist (part (split-string (treesit-node-text node t) ":"))
      (let ((part-end (+ pos (length part))))
        (treesit-fontify-with-override
         pos part-end
         (if root 'beancount-ts-account 'beancount-ts-account-sub)
         override start end)
        (setq root nil
              pos part-end)
        (when (< pos node-end)
          (treesit-fontify-with-override
           pos (1+ pos) 'beancount-ts-account-separator override start end)
          (setq pos (1+ pos)))))))

(defvar beancount-ts--font-lock-settings
  (treesit-font-lock-rules
   :language 'beancount
   :feature 'comment
   '((comment) @font-lock-comment-face)

   :language 'beancount
   :feature 'string
   '([(string) (payee) (narration)] @beancount-ts-string)

   :language 'beancount
   :feature 'date
   '((date) @beancount-ts-date)

   :language 'beancount
   :feature 'number
   '([(number) (minus)] @beancount-ts-amount)

   :language 'beancount
   :feature 'currency
   '((currency) @beancount-ts-currency)

   ;; Keywords are anonymous nodes, so these capture the word alone
   ;; rather than the whole directive.
   :language 'beancount
   :feature 'directive
   '(["include" "option" "plugin" "popmeta" "poptag" "pushmeta" "pushtag"
      "balance" "close" "commodity" "custom" "document" "event" "note"
      "open" "pad" "price" "query"]
     @beancount-ts-directive)

   :language 'beancount
   :feature 'account
   '((account) @beancount-ts--fontify-account)

   :language 'beancount
   :feature 'tag
   '((tag) @beancount-ts-tag)

   :language 'beancount
   :feature 'link
   '((link) @beancount-ts-link)

   ;; `*' is an anonymous node under `txn'; `!' and other flags are a
   ;; named `flag' node.
   :language 'beancount
   :feature 'transaction
   '((txn "*" @beancount-ts-flag-cleared)
     (txn "txn" @beancount-ts-directive)
     (flag) @beancount-ts-flag-pending)

   :language 'beancount
   :feature 'metadata
   '((key) @beancount-ts-metadata-key
     (value) @font-lock-property-use-face))
  "Tree-sitter font-lock settings for `beancount-ts-mode'.")

(defvar beancount-ts--font-lock-feature-list
  '((comment string)
    (date number currency directive)
    (account transaction tag link metadata))
  "Font-lock features by level for `beancount-ts-mode'.
Everything the mode defines is on at Emacs's default
`treesit-font-lock-level' of 3.")

(defvar beancount-ts-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?\; "<" table)   ; ; starts comment
    (modify-syntax-entry ?\n ">" table)   ; newline ends comment
    (modify-syntax-entry ?\" "\"" table)  ; string delimiter
    (modify-syntax-entry ?: "_" table)    ; : joins account components
    (modify-syntax-entry ?# "_" table)    ; #tag is one symbol
    (modify-syntax-entry ?^ "_" table)    ; ^link likewise
    table)
  "Syntax table for `beancount-ts-mode'.")

;;; Indentation

;; Deliberately regexp-based rather than `treesit-simple-indent-rules':
;; on a fresh blank line after a transaction the node at point is nil and
;; its parent is the file, so tree rules would need a previous-line rule
;; that amounts to the regexps below anyway.

(defconst beancount-ts--date-re
  "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
  "Regexp matching a beancount YYYY-MM-DD date.")

(defconst beancount-ts--directive-line-re
  (concat "^[ \t]*\\(?:" beancount-ts--date-re
          "\\|\\(?:option\\|plugin\\|include"
          "\\|pushtag\\|poptag\\|pushmeta\\|popmeta\\)\\_>"
          "\\|\\*\\)")
  "Regexp matching a line that begins a top-level directive.
Such lines (dates, push/pop keywords, org-style `*' headings) sit at
column 0.")

(defconst beancount-ts--transaction-header-re
  (concat "^" beancount-ts--date-re "[ \t]+\\(?:[*!&#?%PSTCURM]\\|txn\\_>\\)\\(?:[ \t]\\|$\\)")
  "Regexp matching a transaction header line: a date, then a flag.
The flag is `txn' or any single character Beancount accepts there,
not only `*' and `!': importers emit P, S and T, and pad emits P.")

(defconst beancount-ts--indented-content-re
  "^[ \t]+[^ \t\n]"
  "Regexp matching an already-indented content line (posting or metadata).")

(defconst beancount-ts--dated-line-re
  (concat "^" beancount-ts--date-re)
  "Regexp matching any dated directive line.")

(defun beancount-ts--compute-indent ()
  "Return the column the current line should be indented to.
Lines that begin a top-level directive sit at column 0.  Lines that
continue an entry get `beancount-ts-indent-offset': any line after a
transaction header or an already-indented line, and a line with
content after any other dated directive, which can only be that
directive's metadata.  A blank line after such a directive stays at
column 0, since a new entry is the likelier continuation there."
  (save-excursion
    (beginning-of-line)
    (cond
     ((looking-at-p beancount-ts--directive-line-re) 0)
     ((let ((content (looking-at-p "^[ \t]*[^ \t\n]")))
        (save-excursion
          (forward-line -1)
          (while (and (not (bobp)) (looking-at-p "^[ \t]*$"))
            (forward-line -1))
          (or (looking-at-p beancount-ts--transaction-header-re)
              (looking-at-p beancount-ts--indented-content-re)
              (and content (looking-at-p beancount-ts--dated-line-re)))))
      beancount-ts-indent-offset)
     (t 0))))

(defun beancount-ts-indent-line ()
  "Indent the current line for `beancount-ts-mode'.
Preserves point when it is past the current indentation."
  (let ((target (beancount-ts--compute-indent))
        (savep (> (current-column) (current-indentation))))
    (if savep
        (save-excursion (indent-line-to target))
      (indent-line-to target))))

;;; Structure

;; Beancount has no defuns, but its entries -- a dated directive plus any
;; indented body -- fill the same structural role, and a posting reads as
;; a sentence. Declaring them as tree-sitter things is what makes
;; `treesit-major-mode-setup' hand the stock vocabulary to every editing
;; state: `C-M-a', `C-M-e', `C-M-h', `narrow-to-defun', `M-e', and the
;; `defun' thing that `bounds-of-thing-at-point' resolves -- which Meow's
;; built-in `defun' thing ('. d') rides on. Tree walks also find entries
;; nested under org-style `*' headings, which the grammar wraps in
;; `section' nodes.

(defconst beancount-ts--scope-boundary-types
  '("pushtag" "poptag" "pushmeta" "popmeta")
  "Entry node types that open or close a tag or metadata scope.
Every entry between a push and its pop carries that tag or metadata,
so a sort must not move an entry across one of these lines.")

(defconst beancount-ts-entry-node-types
  (append '("transaction" "balance" "open" "close" "commodity" "pad" "event"
            "query" "note" "document" "custom" "option" "include" "plugin" "price")
          beancount-ts--scope-boundary-types)
  "Tree-sitter node types that form a top-level beancount entry.")

(defconst beancount-ts--entry-query
  (mapcar (lambda (type) (list (list (intern type)) '@entry))
          beancount-ts-entry-node-types)
  "Query capturing every named entry node, one pattern per type.")

(defconst beancount-ts--entry-regexp
  (rx-to-string `(seq bos (or ,@beancount-ts-entry-node-types) eos) t)
  "Regexp matching exactly the node types in `beancount-ts-entry-node-types'.")

;; A directive's keyword is an anonymous node whose type is the keyword
;; itself -- the same type as the entry that holds it -- so every
;; predicate here insists on a named node.
(defun beancount-ts-transaction-marker (node)
  "Return the marker text of transaction NODE: \"*\", \"!\", \"txn\" or a flag."
  (when-let* ((marker (treesit-node-child-by-field-name node "txn")))
    (treesit-node-text marker t)))

(defun beancount-ts--uncleared-p (node)
  "Return non-nil when transaction NODE is not marked cleared (*)."
  (not (equal (beancount-ts-transaction-marker node) "*")))

(defvar beancount-ts--thing-settings
  `((beancount
     (defun (and named ,beancount-ts--entry-regexp))
     (transaction "\\`transaction\\'")
     (uncleared-transaction ("\\`transaction\\'" . beancount-ts--uncleared-p))
     (sentence "\\`posting\\'")))
  "`treesit-thing-settings' for `beancount-ts-mode'.
`transaction' is the subset of `defun' that the module's motion,
clearing and Meow things act on; `uncleared-transaction' narrows it
again for the clearing run.")

(defun beancount-ts--enclosing-node (pred)
  "Return the innermost node at point that satisfies PRED, or nil.
PRED is anything `treesit-parent-until' accepts: a thing name such as
`defun', a node-type regexp, or a predicate function."
  (when-let* ((node (treesit-node-at (point))))
    (treesit-parent-until node pred t)))

(defun beancount-ts-bounds-of-entry ()
  "Return (BEG . END) of the beancount entry at point, or nil."
  (when-let* ((node (beancount-ts--enclosing-node 'defun)))
    (cons (treesit-node-start node) (treesit-node-end node))))

(defun beancount-ts-bounds-of-transaction ()
  "Return (BEG . END) of the transaction at point, or nil.
Non-transaction directives yield nil, which is what makes this usable
as a Meow thing distinct from `defun'."
  (when-let* ((node (beancount-ts--enclosing-node 'transaction)))
    (cons (treesit-node-start node) (treesit-node-end node))))

(defun beancount-ts-inner-of-transaction ()
  "Return (BEG . END) spanning the postings of the transaction at point.
Excludes the header line (date, flag, payee, narration, tags).  A
transaction with no postings falls back to its full bounds."
  (when-let* ((node (beancount-ts--enclosing-node 'transaction)))
    (let ((postings (treesit-filter-child
                     node (lambda (n) (equal (treesit-node-type n) "posting")))))
      (if postings
          (cons (treesit-node-start (car postings))
                (treesit-node-end (car (last postings))))
        (cons (treesit-node-start node) (treesit-node-end node))))))

(defun beancount-ts-entries-in-region (beg end)
  "Return the entry nodes lying wholly between BEG and END, in buffer order.
Entries are the `defun' things: dated directives and the undated
top-level ones.  Text that is not an entry -- comments, blank lines,
org-style headings -- is not represented, so callers that rewrite a
region must carry it themselves."
  (seq-filter (lambda (node)
                (and (>= (treesit-node-start node) beg)
                     (<= (treesit-node-end node) end)))
              (treesit-query-capture
               (treesit-buffer-root-node 'beancount)
               beancount-ts--entry-query beg end t)))

;;; Names and imenu

(defun beancount-ts--field-text (node field)
  "Return the text of NODE's FIELD child, or nil when it has none."
  (when-let* ((child (treesit-node-child-by-field-name node field)))
    (treesit-node-text child t)))

(defun beancount-ts--unquote (string)
  "Return STRING without surrounding double quotes."
  (string-trim string "\"" "\""))

(defun beancount-ts--defun-name (node)
  "Return a name for NODE, for imenu and `which-function-mode'.
Sections are named by their headline, account directives by their
account, and transactions by date, payee and narration."
  (pcase (treesit-node-type node)
    ("section"
     (when-let* ((headline (treesit-node-child-by-field-name node "headline")))
       (beancount-ts--field-text headline "item")))
    ("transaction"
     (let ((payee (beancount-ts--field-text node "payee"))
           (narration (beancount-ts--field-text node "narration")))
       (string-join
        (delq nil (list (beancount-ts--field-text node "date")
                        (and payee (concat (beancount-ts--unquote payee) ":"))
                        (and narration (beancount-ts--unquote narration))))
        " ")))
    ((or "open" "close" "balance" "pad" "note" "document")
     (beancount-ts--field-text node "account"))))

(defun beancount-ts--named-p (node)
  "Return non-nil when NODE is a named node."
  (treesit-node-check node 'named))

(defvar beancount-ts--imenu-settings
  '(("Section" "\\`section\\'" beancount-ts--named-p nil)
    ("Open" "\\`open\\'" beancount-ts--named-p nil)
    ("Transaction" "\\`transaction\\'" beancount-ts--named-p nil))
  "`treesit-simple-imenu-settings' for `beancount-ts-mode'.")

;;; Mode

;;;###autoload
(define-derived-mode beancount-ts-mode prog-mode "Beancount"
  "Major mode for editing Beancount files with tree-sitter.

\\{beancount-ts-mode-map}"
  :group 'beancount-ts
  :syntax-table beancount-ts-mode-syntax-table
  ;; Not being able to parse is an error, not a silent bare mode: the
  ;; stock `(when (and (treesit-ensure-installed ..) (treesit-ready-p
  ;; ..)))' shape returns nil without a word when the install is
  ;; declined or fails, or the buffer is over `treesit-max-buffer-size',
  ;; and would leave a buffer labelled Beancount with no font-lock,
  ;; indentation or navigation.
  (unless (treesit-ensure-installed 'beancount)
    (error "Tree-sitter grammar for beancount is not available"))
  (unless (treesit-ready-p 'beancount 'message)
    (error "Tree-sitter cannot parse this buffer (see the warning above)"))
  (setq treesit-primary-parser (treesit-parser-create 'beancount))
  ;; Comments.
  (setq-local comment-start "; ")
  (setq-local comment-end "")
  ;; Font-lock.
  (setq-local treesit-font-lock-settings beancount-ts--font-lock-settings)
  (setq-local treesit-font-lock-feature-list beancount-ts--font-lock-feature-list)
  ;; Indentation: posting lines indent to `beancount-ts-indent-offset'.
  ;; Drives RET (electric-indent), TAB, and evil o/O (indent-according-to-mode).
  (setq-local indent-line-function #'beancount-ts-indent-line)
  ;; Structure: entries are defuns, postings are sentences.
  (setq-local treesit-thing-settings beancount-ts--thing-settings)
  (setq-local treesit-defun-name-function #'beancount-ts--defun-name)
  (setq-local treesit-simple-imenu-settings beancount-ts--imenu-settings)
  ;; Outline: org-style `*' headings, levelled by their stars.  Set
  ;; before `treesit-major-mode-setup', which would otherwise derive an
  ;; outline from the imenu settings.  The tree is no better here: a
  ;; heading line starts where the previous entry's node ends, and
  ;; `treesit-outline-level' resolves that boundary to the wrong
  ;; `section', flattening nested headings.
  (setq-local outline-regexp "[*]+")
  (setq-local outline-level (lambda () (length (match-string 0))))
  (treesit-major-mode-setup))

(derived-mode-add-parents 'beancount-ts-mode '(beancount-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.beancount\\'" . beancount-ts-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.bean\\'" . beancount-ts-mode))

;;; Navigation

;; Entry motion is `beginning-of-defun' / `end-of-defun' (the mode makes
;; entries the `defun' thing), so only transactions need commands of
;; their own.  `treesit-beginning-of-thing' takes the count with the
;; sign convention of `beginning-of-defun': positive moves back.

;;;###autoload
(defun beancount-ts-next-transaction (&optional count)
  "Move to the start of the next transaction, COUNT times."
  (interactive "p")
  (treesit-beginning-of-thing 'transaction (- (or count 1))))

;;;###autoload
(defun beancount-ts-prev-transaction (&optional count)
  "Move to the start of the previous transaction, COUNT times."
  (interactive "p")
  (treesit-beginning-of-thing 'transaction (or count 1)))

;;; LSP

(defun beancount-ts-eglot-init-options (_server)
  "Return beancount-language-server's initialization options.
The defcustom allows nil; the server cannot work without a journal, so
say so plainly rather than let `expand-file-name' choke on nil.  The
flags go out as a vector so an empty list reaches the server as `[]',
which switches its native flagged-entry scan off, rather than as
`null', which would leave the server's default in force."
  (unless beancount-ts-journal-file
    (user-error "Set `beancount-ts-journal-file' before starting beancount-language-server"))
  `(:journal_file ,(expand-file-name beancount-ts-journal-file)
    :diagnostic_flags ,(vconcat beancount-ts-diagnostic-flags)))

;;; Transaction operations

(defun beancount-ts--transaction-at-point ()
  "Return the transaction node at point, or signal a `user-error'."
  (or (treesit-parent-until (treesit-node-at (point)) 'transaction t)
      (user-error "Not in a transaction")))

(defun beancount-ts--set-marker (txn flag)
  "Rewrite the marker of transaction node TXN to FLAG.
The marker is the `txn' field, which holds `*', `!' or the word `txn'."
  (let ((marker (treesit-node-child-by-field-name txn "txn")))
    (unless marker
      (user-error "Could not find transaction flag"))
    (save-excursion
      (goto-char (treesit-node-start marker))
      (delete-region (point) (treesit-node-end marker))
      (insert flag))))

;;;###autoload
(defun beancount-ts-transaction-clear (&optional arg)
  "Mark the transaction at point cleared (*).
With prefix ARG, mark it pending (!) instead.  With an active region,
mark every transaction inside it -- the selection is the argument, as
with `beancount-ts-sort'."
  (interactive "P")
  (let ((flag (if arg "!" "*")))
    (if (use-region-p)
        (let ((txns (seq-filter (lambda (n) (equal (treesit-node-type n) "transaction"))
                                (beancount-ts-entries-in-region
                                 (region-beginning) (region-end)))))
          (unless txns
            (user-error "No transactions in the selection"))
          ;; Last first, so earlier nodes keep their positions.
          (dolist (txn (reverse txns))
            (beancount-ts--set-marker txn flag))
          (message "Marked %d transaction%s %s" (length txns)
                   (if (= 1 (length txns)) "" "s") flag))
      (beancount-ts--set-marker (beancount-ts--transaction-at-point) flag))))

;;;###autoload
(defun beancount-ts-next-uncleared-transaction (&optional count)
  "Move to the next transaction not marked `*', COUNT times.
Stays put with a message when there is none."
  (interactive "p")
  (unless (treesit-beginning-of-thing 'uncleared-transaction (- (or count 1)))
    (message "No more uncleared transactions")))

;;;###autoload
(defun beancount-ts-transaction-clear-and-next ()
  "Clear the transaction at point and move to the next uncleared transaction."
  (interactive)
  (beancount-ts-transaction-clear)
  (beancount-ts-next-uncleared-transaction))

;;;###autoload
(defun beancount-ts-clone-transaction ()
  "Insert a copy of the transaction at point below it, point on its date.
One blank line separates the copy from the original and from whatever
follows; an existing blank line is not doubled."
  (interactive)
  (let* ((txn (beancount-ts--transaction-at-point))
         (end (treesit-node-end txn))
         (text (treesit-node-text txn t)))
    (unless (string-suffix-p "\n" text)
      (setq text (concat text "\n")))
    (goto-char end)
    (unless (bolp) (insert "\n"))
    (insert "\n")
    (let ((copy-start (point)))
      (insert text)
      (unless (or (eobp) (looking-at-p "[ \t]*\n"))
        (insert "\n"))
      (goto-char copy-start))))

;;; Sorting

;;;###autoload
(defun beancount-ts-sort ()
  "Sort entries by date: the active selection, or else the whole buffer.
Meow's premise is that the selection is the argument, so one key covers
what region/buffer variants split in two."
  (interactive)
  (if (use-region-p)
      (beancount-ts-sort-region (region-beginning) (region-end))
    (beancount-ts-sort-buffer)))

;;;###autoload
(defun beancount-ts-sort-buffer ()
  "Sort all entries in the buffer by date."
  (interactive)
  (beancount-ts-sort-region (point-min) (point-max)))

;;;###autoload
(defun beancount-ts-sort-region (start end)
  "Sort the entries between START and END by date.
Entries trade places; everything that is not an entry -- comments,
blank lines, org-style headings -- stays exactly where it is.  So an
entry can move across a heading, but no text is ever dropped.
Undated entries (option, include, ...) hold their slot."
  (interactive "r")
  (let ((rewrites (beancount-ts--sort-rewrites
                   (beancount-ts-entries-in-region start end))))
    ;; Rewrite from the last slot backwards so earlier positions stay valid.
    (save-excursion
      (pcase-dolist (`((,beg . ,end) . ,text) (reverse rewrites))
        (goto-char beg)
        (delete-region beg end)
        (insert text)))))

(defun beancount-ts--sort-rewrites (entries)
  "Return the rewrites that sort the dated ENTRIES by date, in buffer order.
Each is ((BEG . END) . TEXT): the slot an entry occupies now and the
text of the entry that belongs there.  A pushtag, poptag, pushmeta or
popmeta line splits the entries into stretches that are sorted
independently, so no entry leaves or enters a scope.  Positions are
read before any rewrite, so callers may edit freely afterwards."
  (let (stretches stretch)
    (dolist (node entries)
      (cond ((member (treesit-node-type node) beancount-ts--scope-boundary-types)
             (when stretch (push (nreverse stretch) stretches))
             (setq stretch nil))
            ((treesit-node-child-by-field-name node "date")
             (push node stretch))))
    (when stretch (push (nreverse stretch) stretches))
    (mapcan (lambda (dated)
              (let ((slots (mapcar (lambda (n)
                                     (cons (treesit-node-start n) (treesit-node-end n)))
                                   dated))
                    (sorted (sort (mapcar (lambda (n)
                                            (cons (treesit-node-text
                                                   (treesit-node-child-by-field-name n "date") t)
                                                  (treesit-node-text n t)))
                                          dated)
                                  (lambda (a b) (string< (car a) (car b))))))
                (seq-mapn (lambda (slot entry) (cons slot (cdr entry)))
                          slots sorted)))
            (nreverse stretches))))

;;; eglot

;; Keyed on this mode, not on the `beancount-mode' it declares as a
;; parent: `add-to-list' prepends and eglot takes the first entry whose
;; mode matches, so an entry keyed on `beancount-mode' would sit ahead
;; of whatever upstream beancount-mode users registered for themselves
;; and hand them an init function that errors when
;; `beancount-ts-journal-file' is nil.  The language id eglot derives
;; is the same either way (it strips the `-ts' suffix).  The options
;; are a function so the journal file is read when the server starts,
;; not when this file loads.
(defvar eglot-server-programs)
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(beancount-ts-mode . ("beancount-language-server" "--stdio"
                                      :initializationOptions
                                      beancount-ts-eglot-init-options))))

(provide 'beancount-ts-mode)
;;; beancount-ts-mode.el ends here
