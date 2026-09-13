;;; beancount-ts-mode-test.el --- Tests for beancount-ts-mode -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'imenu)
(require 'outline)

(defvar eglot-server-programs)

(defconst beancount-ts-test--package-directory
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))
  "The directory holding the package sources.")

(add-to-list 'load-path beancount-ts-test--package-directory)
(require 'beancount-ts-mode)

(defun beancount-ts-test--indent-at (text line)
  "Return computed indent for LINE after inserting TEXT in a temp buffer."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (forward-line (1- line))
    (beancount-ts--compute-indent)))

(ert-deftest beancount-ts-indent/offset-defaults-to-two ()
  "Posting indent offset matches the journals' 2-space convention."
  (should (boundp 'beancount-ts-indent-offset))
  (should (= beancount-ts-indent-offset 2)))

(ert-deftest beancount-ts-indent/blank-line-after-header ()
  "A fresh line after a transaction header indents to the offset."
  (should (= 2 (beancount-ts-test--indent-at
                "2024-01-01 * \"Payee\" \"Narration\"\n" 2))))

(ert-deftest beancount-ts-indent/blank-line-after-posting ()
  "A fresh line after a posting indents to the offset (continue the txn)."
  (should (= 2 (beancount-ts-test--indent-at
                "2024-01-01 * \"Payee\"\n  Assets:Cash  10.00 USD\n" 3))))

(ert-deftest beancount-ts-indent/transaction-header-stays-at-zero ()
  "A transaction header line sits at column 0."
  (should (= 0 (beancount-ts-test--indent-at
                "2024-01-01 * \"Payee\"\n  Assets:Cash\n" 1))))

(ert-deftest beancount-ts-indent/unindented-posting-reindents-to-offset ()
  "A posting typed at column 0 (TAB scenario) reindents to the offset."
  (should (= 2 (beancount-ts-test--indent-at
                "2024-01-01 * \"Payee\"\nAssets:Cash  10.00 USD\n" 2))))

(ert-deftest beancount-ts-indent/top-level-directive-stays-at-zero ()
  "A top-level keyword directive sits at column 0."
  (should (= 0 (beancount-ts-test--indent-at
                "option \"title\" \"Ledger\"\n" 1))))

(ert-deftest beancount-ts-indent/blank-after-open-directive-stays-at-zero ()
  "A non-transaction dated directive does not pull the next line in."
  (should (= 0 (beancount-ts-test--indent-at
                "2024-01-01 open Assets:Cash\n" 2))))

(ert-deftest beancount-ts-indent/metadata-under-open-directive-indents ()
  "A metadata line under open, balance or any dated directive is a
continuation and indents, though a blank line after them does not."
  (should (= 2 (beancount-ts-test--indent-at
                "2024-01-01 open Assets:Cash\n  currency: \"USD\"\n" 2)))
  (should (= 2 (beancount-ts-test--indent-at
                "2024-01-01 balance Assets:Cash  1 USD\nnote: \"x\"\n" 2)))
  (should (= 2 (beancount-ts-test--indent-at
                "2024-01-01 open Assets:Cash\n  currency: \"USD\"\n  note: \"x\"\n" 3))))

(ert-deftest beancount-ts-indent/blank-line-after-any-flag-indents ()
  "Every flag Beancount accepts heads a transaction, not only * and !.
Importers emit P, S and T; a fresh line after any of them continues
the transaction."
  (dolist (flag '("P" "S" "T" "C" "U" "R" "M" "#" "&" "?" "%"))
    (should (= 2 (beancount-ts-test--indent-at
                  (format "2024-01-01 %s \"x\"\n" flag) 2)))))

(ert-deftest beancount-ts-indent/org-heading-stays-at-zero ()
  "An org-style section heading sits at column 0."
  (should (= 0 (beancount-ts-test--indent-at "* Section\n" 1))))

(ert-deftest beancount-ts-indent/first-blank-line-stays-at-zero ()
  "A blank line with no preceding content indents to column 0."
  (should (= 0 (beancount-ts-test--indent-at "\n" 1))))

(ert-deftest beancount-ts-indent/indent-line-applies-offset ()
  "`beancount-ts-indent-line' actually indents a fresh posting line."
  (with-temp-buffer
    (insert "2024-01-01 * \"Payee\"\n")
    (goto-char (point-max))
    (beancount-ts-indent-line)
    (should (= 2 (current-indentation)))))

;;; Structural navigation

;; Batch Emacs sees a bare `user-emacs-directory', so it never finds the
;; grammar Doom installed under its own cache dir. Add that dir when it
;; exists; the tests below skip themselves when the grammar is missing, so
;; `make test-el' stays green on a machine that never ran
;; `treesit-install-language-grammar'.
(let ((dir (expand-file-name "~/.emacs.d/.local/cache/tree-sitter")))
  (when (file-directory-p dir)
    (add-to-list 'treesit-extra-load-path dir)))

(defconst beancount-ts-test--ledger
  (concat "option \"title\" \"Ledger\"\n"
          "\n"
          "2024-01-01 open Assets:Cash\n"
          "\n"
          "2024-01-02 * \"Payee\" \"Narr\"\n"
          "  Assets:Cash    10.00 USD\n"
          "  Expenses:Food\n"
          "\n"
          "2024-01-03 ! \"Other\"\n"
          "  Assets:Cash    -5.00 USD\n"
          "  Expenses:Fun\n")
  "A ledger exercising directives, a cleared txn, and a pending txn.")

(defmacro beancount-ts-test--in-ledger (&rest body)
  "Run BODY in a `beancount-ts-mode' buffer holding the sample ledger."
  (declare (indent 0))
  `(progn
     (skip-unless (treesit-ready-p 'beancount t))
     (with-temp-buffer
       (insert beancount-ts-test--ledger)
       (beancount-ts-mode)
       (goto-char (point-min))
       ,@body)))

(defun beancount-ts-test--text (bounds)
  "Return the buffer text spanned by BOUNDS, a (BEG . END) cons."
  (buffer-substring-no-properties (car bounds) (cdr bounds)))

(ert-deftest beancount-ts-nav/bounds-of-transaction-from-posting ()
  "Point inside a posting yields the whole enclosing transaction."
  (beancount-ts-test--in-ledger
    (search-forward "Expenses:Food")
    (let ((text (beancount-ts-test--text (beancount-ts-bounds-of-transaction))))
      (should (string-prefix-p "2024-01-02 * \"Payee\"" text))
      (should (string-match-p "Expenses:Food" text))
      (should-not (string-match-p "2024-01-03" text)))))

(ert-deftest beancount-ts-nav/bounds-of-transaction-nil-outside ()
  "A non-transaction directive has no transaction bounds."
  (beancount-ts-test--in-ledger
    (search-forward "open Assets:Cash")
    (should-not (beancount-ts-bounds-of-transaction))))

(ert-deftest beancount-ts-nav/inner-of-transaction-is-postings ()
  "The inner range covers the postings but not the header line."
  (beancount-ts-test--in-ledger
    (search-forward "Expenses:Food")
    (let ((text (beancount-ts-test--text (beancount-ts-inner-of-transaction))))
      (should (string-prefix-p "Assets:Cash" (string-trim-left text)))
      (should (string-match-p "Expenses:Food" text))
      (should-not (string-match-p "Payee" text)))))

(ert-deftest beancount-ts-nav/bounds-of-entry-covers-non-transactions ()
  "Entry bounds work on directives that are not transactions."
  (beancount-ts-test--in-ledger
    (search-forward "open Assets:Cash")
    (should (equal "2024-01-01 open Assets:Cash"
                   (string-trim (beancount-ts-test--text
                                 (beancount-ts-bounds-of-entry)))))))

(ert-deftest beancount-ts-nav/beginning-of-entry-moves-to-entry-start ()
  "`beginning-of-defun' lands on the start of the enclosing entry."
  (beancount-ts-test--in-ledger
    (search-forward "Expenses:Food")
    (beginning-of-defun)
    (should (looking-at-p "2024-01-02 \\* \"Payee\""))))

(ert-deftest beancount-ts-nav/beginning-of-entry-repeats-with-arg ()
  "A count moves back that many entries."
  (beancount-ts-test--in-ledger
    (search-forward "Expenses:Fun")
    (beginning-of-defun 2)
    (should (looking-at-p "2024-01-02 \\* \"Payee\""))))

(ert-deftest beancount-ts-nav/end-of-entry-moves-past-last-posting ()
  "`end-of-defun' lands after the final posting of the entry."
  (beancount-ts-test--in-ledger
    (search-forward "2024-01-02")
    (end-of-defun)
    (should (string-match-p
             "\\`[ \t\n]*2024-01-03"
             (buffer-substring-no-properties (point) (point-max))))))

(ert-deftest beancount-ts-nav/mode-wires-defun-functions ()
  "The mode hands defun movement to tree-sitter's `defun' thing."
  (beancount-ts-test--in-ledger
    (should (eq beginning-of-defun-function #'treesit-beginning-of-defun))
    (should (eq end-of-defun-function #'treesit-end-of-defun))))

(ert-deftest beancount-ts-nav/defun-thing-selects-entry ()
  "`bounds-of-thing-at-point' resolves `defun' to the entry.
This is what makes Meow's built-in `defun' thing (\\=`. d\\=') work."
  (beancount-ts-test--in-ledger
    (search-forward "Expenses:Food")
    (let ((text (beancount-ts-test--text (bounds-of-thing-at-point 'defun))))
      (should (string-match-p "2024-01-02 \\* \"Payee\"" text))
      (should (string-match-p "Expenses:Food" text)))))

;;; Org headings

;; The grammar nests everything after a `*' heading inside a `section'
;; node, so entries stop being children of the root there. Navigation
;; must find them regardless.

(defconst beancount-ts-test--headed-ledger
  (concat "* A\n"
          "2024-01-01 open Assets:Cash USD\n"
          "** B\n"
          "2024-01-02 * \"P\" \"N\"\n"
          "  flag: TRUE\n"
          "  who: \"me\"\n"
          "  Assets:Cash  1 USD\n"
          "  Expenses:X\n"
          "2024-01-03 txn \"Bare\"\n"
          "  Assets:Cash  -1 USD\n"
          "  Expenses:Y\n"
          "2024-01-04 ! \"Pending\"\n"
          "  Assets:Cash\n"
          "* C\n"
          "2024-01-05 close Assets:Cash\n")
  "A ledger whose entries all sit under org-style headings.")

(defmacro beancount-ts-test--in-headed-ledger (&rest body)
  "Run BODY in a `beancount-ts-mode' buffer holding the headed ledger."
  (declare (indent 0))
  `(progn
     (skip-unless (treesit-ready-p 'beancount t))
     (with-temp-buffer
       (insert beancount-ts-test--headed-ledger)
       (beancount-ts-mode)
       (goto-char (point-min))
       ,@body)))

(ert-deftest beancount-ts-nav/beginning-of-entry-under-heading ()
  "`beginning-of-defun' finds an entry nested inside a section."
  (beancount-ts-test--in-headed-ledger
    (search-forward "Expenses:X")
    (beginning-of-defun)
    (should (looking-at-p "2024-01-02 \\* \"P\""))))

(ert-deftest beancount-ts-nav/defun-thing-under-heading ()
  "The `defun' thing resolves inside a section."
  (beancount-ts-test--in-headed-ledger
    (search-forward "Expenses:X")
    (let ((text (beancount-ts-test--text (bounds-of-thing-at-point 'defun))))
      (should (string-prefix-p "2024-01-02" text))
      (should (string-match-p "Expenses:X" text))
      (should-not (string-match-p "2024-01-03" text)))))

(ert-deftest beancount-ts-nav/end-of-entry-under-heading ()
  "`end-of-defun' lands after the last posting of a nested entry."
  (beancount-ts-test--in-headed-ledger
    (search-forward "2024-01-02")
    (end-of-defun)
    (should (string-match-p
             "\\`[ \t\n]*2024-01-03"
             (buffer-substring-no-properties (point) (point-max))))))

(ert-deftest beancount-ts-nav/forward-sentence-walks-postings ()
  "A posting is a sentence: `forward-sentence' stops at its end."
  (beancount-ts-test--in-headed-ledger
    (search-forward "Assets:Cash  1 USD")
    (beginning-of-line)
    (let ((start (point)))
      (forward-sentence)
      (should (equal "  Assets:Cash  1 USD\n"
                     (buffer-substring-no-properties start (point)))))))

(ert-deftest beancount-ts-nav/defun-thing-from-keyword ()
  "The keyword of a directive is an anonymous node of the same type as
its entry; the `defun' thing must still resolve to the whole entry."
  (beancount-ts-test--in-headed-ledger
    (search-forward "open")
    (backward-char 2)
    (should (equal "2024-01-01 open Assets:Cash USD"
                   (string-trim (beancount-ts-test--text
                                 (beancount-ts-bounds-of-entry)))))))

;;; Fontification

(defun beancount-ts-test--face-at (needle &optional offset)
  "Return the face at the start of the first NEEDLE, plus OFFSET."
  (goto-char (point-min))
  (search-forward needle)
  (get-text-property (+ (match-beginning 0) (or offset 0)) 'face))

(ert-deftest beancount-ts-font/account-components ()
  "Root, separator and sub-account get their own faces without jit-lock."
  (beancount-ts-test--in-headed-ledger
    (font-lock-ensure)
    (should (eq 'beancount-ts-account (beancount-ts-test--face-at "Assets:Cash  1")))
    (should (eq 'beancount-ts-account-separator
                (beancount-ts-test--face-at "Assets:Cash  1" 6)))
    (should (eq 'beancount-ts-account-sub
                (beancount-ts-test--face-at "Assets:Cash  1" 7)))))

(ert-deftest beancount-ts-font/directive-keyword-only ()
  "Only the keyword of a directive gets the directive face."
  (beancount-ts-test--in-headed-ledger
    (font-lock-ensure)
    (should (eq 'beancount-ts-directive (beancount-ts-test--face-at "open")))
    (should-not (beancount-ts-test--face-at " open"))
    (should (eq 'beancount-ts-account (beancount-ts-test--face-at "open Assets" 5)))
    (should (eq 'beancount-ts-directive (beancount-ts-test--face-at "close")))))

(ert-deftest beancount-ts-font/transaction-flags ()
  "`*' is cleared, `!' is pending, `txn' is a keyword."
  (beancount-ts-test--in-headed-ledger
    (font-lock-ensure)
    (should (eq 'beancount-ts-flag-cleared (beancount-ts-test--face-at "* \"P\"")))
    (should (eq 'beancount-ts-flag-pending (beancount-ts-test--face-at "! \"Pending\"")))
    (should (eq 'beancount-ts-directive (beancount-ts-test--face-at "txn \"Bare\"")))))

(ert-deftest beancount-ts-font/metadata ()
  "Metadata keys and bare values are fontified; string values stay strings."
  (beancount-ts-test--in-headed-ledger
    (font-lock-ensure)
    (should (eq 'beancount-ts-metadata-key (beancount-ts-test--face-at "flag:")))
    (should (eq 'font-lock-property-use-face (beancount-ts-test--face-at "TRUE")))
    (should (eq 'beancount-ts-string (beancount-ts-test--face-at "\"me\"")))))

;;; Mode plumbing

(ert-deftest beancount-ts-mode/derives-from-beancount-mode ()
  "Packages keyed on `beancount-mode' see this mode as one."
  (beancount-ts-test--in-headed-ledger
    (should (derived-mode-p 'beancount-mode))))

(ert-deftest beancount-ts-imenu/index-by-kind ()
  "Imenu lists sections, opened accounts and transactions."
  (beancount-ts-test--in-headed-ledger
    (let* ((index (funcall imenu-create-index-function))
           (names (lambda (kind) (mapcar #'car (cdr (assoc kind index)))))
           (sections (cdr (assoc "Section" index))))
      (should (member "A" (funcall names "Section")))
      ;; B nests under A, as the headings do.
      (should (assoc "B" (cdr (assoc "A" sections))))
      ;; The `open' keyword is an anonymous node of type "open" too; it
      ;; must not surface as a child entry.
      (should (equal '("Assets:Cash") (funcall names "Open")))
      (should (markerp (cdr (assoc "Assets:Cash" (cdr (assoc "Open" index))))))
      (should (seq-find (lambda (n) (string-match-p "P.*N" n))
                        (funcall names "Transaction"))))))

(ert-deftest beancount-ts-outline/level-from-nesting ()
  "Outline headings come from `section' nodes, levelled by nesting."
  (beancount-ts-test--in-headed-ledger
    (outline-next-heading)
    (should (looking-at-p "\\*\\* B"))
    (should (= 2 (funcall outline-level)))
    (outline-next-heading)
    (should (looking-at-p "\\* C"))
    (should (= 1 (funcall outline-level)))))

(ert-deftest beancount-ts-nav/transaction-thing-crosses-headings ()
  "`treesit-beginning-of-thing' on `transaction' walks into sections."
  (beancount-ts-test--in-headed-ledger
    (treesit-beginning-of-thing 'transaction -1)
    (should (looking-at-p "2024-01-02 \\* \"P\""))
    (treesit-beginning-of-thing 'transaction -1)
    (should (looking-at-p "2024-01-03 txn"))
    (treesit-beginning-of-thing 'transaction 1)
    (should (looking-at-p "2024-01-02 \\* \"P\""))))

(ert-deftest beancount-ts-nav/entry-query-is-compiled-once ()
  "The entry query is compiled at load, not rebuilt on every capture.
Sort, region clear and refile all capture with it."
  (should (treesit-compiled-query-p beancount-ts--entry-query)))

(ert-deftest beancount-ts-nav/entries-in-region-in-order ()
  "Entries fully inside the region come back in buffer order, nothing else."
  (beancount-ts-test--in-headed-ledger
    (let ((types (mapcar #'treesit-node-type
                         (beancount-ts-entries-in-region (point-min) (point-max)))))
      (should (equal '("open" "transaction" "transaction" "transaction" "close")
                     types)))))

(ert-deftest beancount-ts-nav/entries-in-region-excludes-partial ()
  "An entry straddling the region edge is left out."
  (beancount-ts-test--in-headed-ledger
    (search-forward "Expenses:X")
    (let ((types (mapcar #'treesit-node-type
                         (beancount-ts-entries-in-region (point) (point-max)))))
      (should (equal '("transaction" "transaction" "close") types)))))

(ert-deftest beancount-ts-nav/uncleared-transaction-thing ()
  "`uncleared-transaction' skips `*' and stops on `txn' and `!'."
  (beancount-ts-test--in-headed-ledger
    (treesit-beginning-of-thing 'uncleared-transaction -1)
    (should (looking-at-p "2024-01-03 txn"))
    (treesit-beginning-of-thing 'uncleared-transaction -1)
    (should (looking-at-p "2024-01-04 !"))
    (let ((here (point)))
      (should-not (treesit-beginning-of-thing 'uncleared-transaction -1))
      (should (= here (point))))))

(ert-deftest beancount-ts-syntax/tags-and-links-are-symbols ()
  "`#' and `^' belong to the tag or link, so the symbol at point is whole."
  (beancount-ts-test--in-headed-ledger
    (erase-buffer)
    (insert "2024-01-01 * \"x\" #food ^receipt-1\n")
    (search-backward "#food")
    (should (equal "#food" (thing-at-point 'symbol t)))
    (search-forward "^rec")
    (should (equal "^receipt-1" (thing-at-point 'symbol t)))))

(ert-deftest beancount-ts-mode/missing-grammar-is-an-error ()
  "A declined or failed grammar install must not leave a silent bare mode."
  (cl-letf (((symbol-function 'treesit-ensure-installed) (lambda (_lang) nil)))
    (with-temp-buffer
      (should-error (beancount-ts-mode) :type 'error))))

;;; Buffer commands

(defconst beancount-ts-cmd-test--ledger
  (concat "* Imported\n"
          "; keep me\n"
          "2024-01-03 * \"three\"\n"
          "  Assets:Cash  3 USD\n"
          "  Expenses:X\n"
          "2024-01-01 balance Assets:Cash 0 USD\n"
          "** Later\n"
          "2024-01-02 txn \"two\"\n"
          "  Assets:Cash  2 USD\n"
          "  Expenses:X\n"
          "\n"
          "2024-01-04 ! \"four\"\n"
          "  Assets:Cash  4 USD\n"
          "  Expenses:X\n")
  "Out-of-order entries under headings, with a directive hard against
a transaction and a comment: everything the sorter must not lose.")

(defmacro beancount-ts-cmd-test--in-ledger (&rest body)
  "Run BODY in a `beancount-ts-mode' buffer holding the command ledger."
  (declare (indent 0))
  `(progn
     (skip-unless (treesit-ready-p 'beancount t))
     (with-temp-buffer
       (insert beancount-ts-cmd-test--ledger)
       (beancount-ts-mode)
       (goto-char (point-min))
       ,@body)))

(defun beancount-ts-cmd-test--line ()
  "Return the current line's text."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(ert-deftest beancount-ts-sort/orders-entries-by-date ()
  "Entries come out date-ascending."
  (beancount-ts-cmd-test--in-ledger
    (beancount-ts-sort-buffer)
    (let ((dates (let (acc)
                   (goto-char (point-min))
                   (while (re-search-forward "^\\(2024-01-0[0-9]\\)" nil t)
                     (push (match-string 1) acc))
                   (nreverse acc))))
      (should (equal '("2024-01-01" "2024-01-02" "2024-01-03" "2024-01-04") dates)))))

(ert-deftest beancount-ts-sort/keeps-directive-hard-against-transaction ()
  "A directive on the line right after a transaction survives the sort."
  (beancount-ts-cmd-test--in-ledger
    (beancount-ts-sort-buffer)
    (should (string-match-p "^2024-01-01 balance Assets:Cash 0 USD$" (buffer-string)))))

(ert-deftest beancount-ts-sort/keeps-comments-and-headings ()
  "Text that is not an entry stays where it was."
  (beancount-ts-cmd-test--in-ledger
    (beancount-ts-sort-buffer)
    (should (string-prefix-p "* Imported\n; keep me\n" (buffer-string)))
    (should (string-match-p "^\\*\\* Later$" (buffer-string)))))

(ert-deftest beancount-ts-sort/region-leaves-outside-alone ()
  "Only entries wholly inside the region move."
  (beancount-ts-cmd-test--in-ledger
    (search-forward "** Later")
    (beancount-ts-sort-region (line-beginning-position) (point-max))
    (goto-char (point-min))
    (should (re-search-forward "^2024-01-03" nil t))
    (should (re-search-forward "^2024-01-01" nil t))
    (should (re-search-forward "^2024-01-02" nil t))
    (should (re-search-forward "^2024-01-04" nil t))))

(ert-deftest beancount-ts-nav/next-transaction-crosses-heading ()
  "`beancount-ts-next-transaction' skips directives and headings."
  (beancount-ts-cmd-test--in-ledger
    (beancount-ts-next-transaction)
    (should (string-prefix-p "2024-01-03 *" (beancount-ts-cmd-test--line)))
    (beancount-ts-next-transaction)
    (should (string-prefix-p "2024-01-02 txn" (beancount-ts-cmd-test--line)))))

(ert-deftest beancount-ts-nav/transaction-motion-takes-count ()
  "A count moves that many transactions, in either direction."
  (beancount-ts-cmd-test--in-ledger
    (beancount-ts-next-transaction 3)
    (should (string-prefix-p "2024-01-04 !" (beancount-ts-cmd-test--line)))
    (beancount-ts-prev-transaction 2)
    (should (string-prefix-p "2024-01-03 *" (beancount-ts-cmd-test--line)))))

(ert-deftest beancount-ts-clear/sets-cleared-flag ()
  "Clearing rewrites `txn' and `!' markers to `*'."
  (beancount-ts-cmd-test--in-ledger
    (search-forward "txn \"two\"")
    (beancount-ts-transaction-clear)
    (should (equal "2024-01-02 * \"two\"" (beancount-ts-cmd-test--line)))
    (search-forward "! \"four\"")
    (beancount-ts-transaction-clear)
    (should (equal "2024-01-04 * \"four\"" (beancount-ts-cmd-test--line)))))

(ert-deftest beancount-ts-clear/prefix-sets-pending ()
  "With a prefix argument the marker becomes `!'."
  (beancount-ts-cmd-test--in-ledger
    (search-forward "Assets:Cash  3")
    (beancount-ts-transaction-clear t)
    (forward-line -1)
    (should (equal "2024-01-03 ! \"three\"" (beancount-ts-cmd-test--line)))))

(ert-deftest beancount-ts-clear/outside-transaction-errors ()
  "Clearing a non-transaction directive is a `user-error'."
  (beancount-ts-cmd-test--in-ledger
    (search-forward "balance")
    (should-error (beancount-ts-transaction-clear) :type 'user-error)))


;;; Uncleared motion

(ert-deftest beancount-ts-nav/next-uncleared-skips-cleared ()
  "`beancount-ts-next-uncleared-transaction' lands on `txn' and `!' only."
  (beancount-ts-cmd-test--in-ledger
    (beancount-ts-next-uncleared-transaction)
    (should (string-prefix-p "2024-01-02 txn" (beancount-ts-cmd-test--line)))
    (beancount-ts-next-uncleared-transaction)
    (should (string-prefix-p "2024-01-04 !" (beancount-ts-cmd-test--line)))
    (let ((here (point)))
      (beancount-ts-next-uncleared-transaction)
      (should (= here (point))))))

(ert-deftest beancount-ts-nav/next-uncleared-takes-count ()
  "A count skips that many uncleared transactions."
  (beancount-ts-cmd-test--in-ledger
    (beancount-ts-next-uncleared-transaction 2)
    (should (string-prefix-p "2024-01-04 !" (beancount-ts-cmd-test--line)))))

;;; Clearing a selection

(ert-deftest beancount-ts-clear/region-clears-every-transaction ()
  "With an active region every transaction inside it is cleared."
  (beancount-ts-cmd-test--in-ledger
    (search-forward "** Later")
    (transient-mark-mode 1)
    (set-mark (line-beginning-position))
    (goto-char (point-max))
    (activate-mark)
    (beancount-ts-transaction-clear)
    (should (string-match-p "^2024-01-02 \\* \"two\"" (buffer-string)))
    (should (string-match-p "^2024-01-04 \\* \"four\"" (buffer-string)))
    (should (string-match-p "^2024-01-03 \\* \"three\"" (buffer-string)))))

(ert-deftest beancount-ts-clear/region-survives-a-reparse-between-edits ()
  "Clearing a selection must not hold tree-sitter nodes across its edits.
A hook that touches the tree after each change makes every node from
before the first edit outdated; the positions have to be taken first."
  (beancount-ts-cmd-test--in-ledger
    (add-hook 'after-change-functions
              (lambda (&rest _) (treesit-buffer-root-node 'beancount))
              nil t)
    (transient-mark-mode 1)
    (set-mark (point-min))
    (goto-char (point-max))
    (activate-mark)
    (beancount-ts-transaction-clear)
    (should (string-match-p "^2024-01-03 \\* \"three\"" (buffer-string)))
    (should (string-match-p "^2024-01-02 \\* \"two\"" (buffer-string)))
    (should (string-match-p "^2024-01-04 \\* \"four\"" (buffer-string)))))

;;; Cloning

(ert-deftest beancount-ts-clone/separates-copy-from-neighbours ()
  "The copy sits below the original with one blank line on each side."
  (skip-unless (treesit-ready-p 'beancount t))
  (with-temp-buffer
    (insert "2024-01-02 * \"a\"\n  A:B 1 USD\n  E:X\n2024-01-03 * \"b\"\n  A:B 1 USD\n")
    (beancount-ts-mode)
    (goto-char (point-min))
    (search-forward "A:B 1 USD")
    (beancount-ts-clone-transaction)
    (should (equal (concat "2024-01-02 * \"a\"\n  A:B 1 USD\n  E:X\n"
                           "\n"
                           "2024-01-02 * \"a\"\n  A:B 1 USD\n  E:X\n"
                           "\n"
                           "2024-01-03 * \"b\"\n  A:B 1 USD\n")
                   (buffer-string)))
    (should (looking-at-p "2024-01-02 \\* \"a\""))
    (should (= 5 (line-number-at-pos)))))

(ert-deftest beancount-ts-clone/does-not-double-blank-lines ()
  "A blank line already after the original is not duplicated."
  (skip-unless (treesit-ready-p 'beancount t))
  (with-temp-buffer
    (insert "2024-01-02 * \"a\"\n  A:B 1 USD\n\n2024-01-03 * \"b\"\n  A:B 1 USD\n")
    (beancount-ts-mode)
    (goto-char (point-min))
    (beancount-ts-clone-transaction)
    (should (equal (concat "2024-01-02 * \"a\"\n  A:B 1 USD\n"
                           "\n"
                           "2024-01-02 * \"a\"\n  A:B 1 USD\n"
                           "\n"
                           "2024-01-03 * \"b\"\n  A:B 1 USD\n")
                   (buffer-string)))))


;;; Tag and metadata scopes

(defconst beancount-ts-scope-test--ledger
  (concat "2024-02-01 * \"before\"\n"
          "  Assets:Cash  1 USD\n"
          "  Expenses:X\n"
          "pushtag #trip\n"
          "2024-03-02 * \"hotel\"\n"
          "  Assets:Cash  1 USD\n"
          "  Expenses:X\n"
          "2024-03-01 * \"flight\"\n"
          "  Assets:Cash  1 USD\n"
          "  Expenses:X\n"
          "poptag #trip\n"
          "\n"
          "2024-01-01 * \"groceries\"\n"
          "  Assets:Cash  1 USD\n"
          "  Expenses:X\n")
  "Out-of-order entries inside and around a pushtag block.")

(ert-deftest beancount-ts-nav/push-and-pop-directives-are-entries ()
  "pushtag, poptag, pushmeta and popmeta lines are entries too, so
motion stops on them and a sort knows where a scope begins and ends."
  (skip-unless (treesit-ready-p 'beancount t))
  (with-temp-buffer
    (insert beancount-ts-scope-test--ledger)
    (beancount-ts-mode)
    (should (equal '("transaction" "pushtag" "transaction" "transaction"
                     "poptag" "transaction")
                   (mapcar #'treesit-node-type
                           (beancount-ts-entries-in-region (point-min) (point-max)))))))

(ert-deftest beancount-ts-sort/keeps-entries-inside-their-tag-scope ()
  "Sorting never moves an entry across a pushtag or poptag line.
Entries are sorted within each stretch between such lines, so the
scope covers the same entries afterwards as before."
  (skip-unless (treesit-ready-p 'beancount t))
  (with-temp-buffer
    (insert beancount-ts-scope-test--ledger)
    (beancount-ts-mode)
    (beancount-ts-sort-buffer)
    (let ((names (let (acc)
                   (goto-char (point-min))
                   (while (re-search-forward "^\\(pushtag\\|poptag\\|[0-9-]+ \\* \"\\([a-z]+\\)\"\\)" nil t)
                     (push (or (match-string 2) (match-string 1)) acc))
                   (nreverse acc))))
      (should (equal '("before" "pushtag" "flight" "hotel" "poptag" "groceries")
                     names)))))

;;; beancount-ts-eglot-init-options

(ert-deftest beancount-ts-eglot/init-options-expands-journal-file ()
  "The journal path reaches the server expanded, not with a tilde."
  (let ((beancount-ts-journal-file "~/accounting/journal/main.beancount")
        (beancount-ts-diagnostic-flags '("!")))
    (should (equal (expand-file-name "~/accounting/journal/main.beancount")
                   (plist-get (beancount-ts-eglot-init-options nil)
                              :journal_file)))))

(ert-deftest beancount-ts-eglot/init-options-sends-diagnostic-flags-as-array ()
  "`beancount-ts-diagnostic-flags' goes out as a JSON array (a vector), so
the server's native flagged-entry scan warns on exactly those flags."
  (let ((beancount-ts-journal-file "/tmp/main.beancount")
        (beancount-ts-diagnostic-flags '("R")))
    (should (equal ["R"]
                   (plist-get (beancount-ts-eglot-init-options nil)
                              :diagnostic_flags)))))

(ert-deftest beancount-ts-eglot/init-options-empty-flags-disable-native-scan ()
  "No flags is still a JSON array, not JSON null: `[]' is how the server's
native scan is switched off."
  (let ((beancount-ts-journal-file "/tmp/main.beancount")
        (beancount-ts-diagnostic-flags nil))
    (should (equal []
                   (plist-get (beancount-ts-eglot-init-options nil)
                              :diagnostic_flags)))))

(ert-deftest beancount-ts-eglot/init-options-without-journal-is-user-error ()
  "A nil journal file is reported plainly rather than expanded."
  (let ((beancount-ts-journal-file nil))
    (should-error (beancount-ts-eglot-init-options nil) :type 'user-error)))

;;; Packaging

(ert-deftest beancount-ts-mode/autoloads-register-the-mode ()
  "The generated autoloads file wires .beancount and .bean files to the mode.
This is what package activation runs; without it a package install
opens journals in `fundamental-mode' while the commands autoload fine."
  ;; The file must not exist yet: `loaddefs-generate' skips sources
  ;; older than its output, and a fresh temp file is newer than all.
  (let ((loaddefs (concat (make-temp-name
                           (expand-file-name "beancount-ts-loaddefs" temporary-file-directory))
                          ".el")))
    (unwind-protect
        (progn
          (loaddefs-generate beancount-ts-test--package-directory loaddefs)
          (let ((text (with-temp-buffer
                        (insert-file-contents loaddefs)
                        (buffer-string))))
            (should (string-search "(autoload 'beancount-ts-mode " text))
            (should (string-search
                     "(add-to-list 'auto-mode-alist '(\"\\\\.beancount\\\\'\" . beancount-ts-mode))"
                     text))
            (should (string-search
                     "(add-to-list 'auto-mode-alist '(\"\\\\.bean\\\\'\" . beancount-ts-mode))"
                     text))))
      (when (file-exists-p loaddefs) (delete-file loaddefs)))))

(ert-deftest beancount-ts-eglot/registers-server-for-this-mode-only ()
  "Loading eglot after the mode leaves a `beancount-ts-mode' entry that
hands the server `beancount-ts-eglot-init-options' for its options.
The entry is not keyed on `beancount-mode': `add-to-list' prepends and
eglot takes the first match, so an entry keyed there would shadow what
upstream beancount-mode users registered for themselves."
  (require 'eglot)
  (let ((entry (assq 'beancount-ts-mode eglot-server-programs)))
    (should entry)
    (should (equal "beancount-language-server" (cadr entry)))
    (should (eq 'beancount-ts-eglot-init-options
                (cadr (memq :initializationOptions (cdr entry)))))
    (should-not (assq 'beancount-mode eglot-server-programs))))

(ert-deftest beancount-ts-mode/oversized-buffer-is-an-error ()
  "A buffer `treesit-ready-p' refuses must not get a silent bare mode either."
  (skip-unless (treesit-ready-p 'beancount t))
  (let ((treesit-max-buffer-size 1))
    (with-temp-buffer
      (insert "2024-01-01 open Assets:Cash\n")
      (should-error (beancount-ts-mode) :type 'error))))

(provide 'beancount-ts-mode-test)
;;; beancount-ts-mode-test.el ends here
