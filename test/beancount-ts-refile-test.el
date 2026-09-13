;;; beancount-ts-refile-test.el --- Tests for beancount-ts-refile -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(add-to-list 'load-path
             (expand-file-name ".." (file-name-directory
                                     (or load-file-name buffer-file-name))))
(require 'beancount-ts-mode)
(require 'beancount-ts-refile)

(defconst beancount-ts-refile-test--journal-files
  '("assets/chase/checking.beancount"
    "assets/vanguard/dustin/roth-ira.beancount"
    "consulting/liabilities/capitalone/spark-visa.beancount"
    "consulting/liabilities/chase/ink-visa.beancount"
    "liabilities/apple/mastercard.beancount"
    "liabilities/capitalone/quicksilver-visa.beancount"
    "liabilities/chase/prime-visa.beancount"
    "prices.beancount")
  "Journal layout mirroring ~/accounting/journal, including the solid
`capitalone'/`mastercard' directories that CamelCase splitting misses.")

;;; beancount-ts--account-to-path-guess

(ert-deftest beancount-ts-refile/path-guess-splits-camel-case ()
  "CamelCase account components become kebab-case path components."
  (should (equal (beancount-ts--account-to-path-guess
                  "Liabilities:Chase:PrimeVisa")
                 "liabilities/chase/prime-visa")))

(ert-deftest beancount-ts-refile/path-guess-hoists-business-entity ()
  "A configured entity in slot two becomes the leading path component."
  (let ((beancount-ts-refile-entities '("Consulting")))
    (should (equal (beancount-ts--account-to-path-guess
                    "Liabilities:Consulting:CapitalOne:SparkVisa")
                   "consulting/liabilities/capital-one/spark-visa"))))

(ert-deftest beancount-ts-refile/path-guess-without-entities-keeps-order ()
  "With no entities configured, slot two is an ordinary path component."
  (let ((beancount-ts-refile-entities nil))
    (should (equal (beancount-ts--account-to-path-guess
                    "Liabilities:Consulting:CapitalOne:SparkVisa")
                   "liabilities/consulting/capital-one/spark-visa"))))

;;; beancount-ts--find-ancestor-file

(ert-deftest beancount-ts-refile/ancestor-file-exact-match ()
  "An exactly-spelled guess resolves to its file."
  (should (equal (beancount-ts--find-ancestor-file
                  "liabilities/chase/prime-visa"
                  beancount-ts-refile-test--journal-files)
                 "liabilities/chase/prime-visa.beancount")))

(ert-deftest beancount-ts-refile/ancestor-file-ignores-hyphenation ()
  "`CapitalOne' resolves to the solid `capitalone' directory on disk."
  (should (equal (beancount-ts--find-ancestor-file
                  "liabilities/capital-one/quicksilver-visa"
                  beancount-ts-refile-test--journal-files)
                 "liabilities/capitalone/quicksilver-visa.beancount")))

(ert-deftest beancount-ts-refile/ancestor-file-ignores-hyphenation-under-entity ()
  "Hyphen-insensitivity also applies below a business entity directory."
  (should (equal (beancount-ts--find-ancestor-file
                  "consulting/liabilities/capital-one/spark-visa"
                  beancount-ts-refile-test--journal-files)
                 "consulting/liabilities/capitalone/spark-visa.beancount")))

(ert-deftest beancount-ts-refile/ancestor-file-truncates-then-ignores-hyphenation ()
  "A sub-account walks up to `mastercard.beancount' despite the split."
  (should (equal (beancount-ts--find-ancestor-file
                  "liabilities/apple/master-card/monthly-installments"
                  beancount-ts-refile-test--journal-files)
                 "liabilities/apple/mastercard.beancount")))

(ert-deftest beancount-ts-refile/ancestor-file-truncates-to-parent ()
  "A holding sub-account resolves to its parent account file."
  (should (equal (beancount-ts--find-ancestor-file
                  "assets/vanguard/dustin/roth-ira/vaigx"
                  beancount-ts-refile-test--journal-files)
                 "assets/vanguard/dustin/roth-ira.beancount")))

(ert-deftest beancount-ts-refile/ancestor-file-unresolvable-is-nil ()
  "An account with no journal file resolves to nil, not a stray match."
  (should-not (beancount-ts--find-ancestor-file
               "liabilities/roundpoint/home-mortgage"
               beancount-ts-refile-test--journal-files)))

(ert-deftest beancount-ts-refile/ancestor-file-stops-above-two-components ()
  "Truncation stops before a bare account-type root file."
  (should-not (beancount-ts--find-ancestor-file
               "prices/whatever"
               beancount-ts-refile-test--journal-files)))

;;; beancount-ts--find-best-file-match

(ert-deftest beancount-ts-refile/best-match-ignores-hyphenation ()
  "The prompt default also tolerates solid directory spellings."
  (should (equal (beancount-ts--find-best-file-match
                  "liabilities/capital-one/quicksilver-visa"
                  beancount-ts-refile-test--journal-files)
                 "liabilities/capitalone/quicksilver-visa.beancount")))

;;; Saving

(defun beancount-ts-refile-test--file-buffer (name &optional poison)
  "Return a buffer visiting a fresh temp file NAME with some content.
With POISON, saving the buffer signals an error from `after-save-hook',
the way a dead LSP server does when eglot notifies it of the save."
  (let* ((path (expand-file-name name (make-temp-file "beancount-ts" t)))
         (buf (find-file-noselect path)))
    (with-current-buffer buf
      (insert "2024-01-01 balance Assets:Cash  1.00 USD\n")
      (when poison
        (add-hook 'after-save-hook
                  (lambda () (error "Simulated save failure")) nil t)))
    buf))

(ert-deftest beancount-ts-refile/save-buffers-reports-failures ()
  "A failing save is reported rather than signalled."
  (let* ((bad (beancount-ts-refile-test--file-buffer "bad.beancount" t))
         (failures (beancount-ts--save-buffers (list bad))))
    (should (equal (mapcar #'car failures) (list bad)))))

(ert-deftest beancount-ts-refile/save-buffers-continues-past-failure ()
  "One buffer failing to save must not strand the buffers behind it.
A batch refile has already removed the entries from the source, so
every target has to get its chance to reach disk."
  (let* ((bad (beancount-ts-refile-test--file-buffer "bad.beancount" t))
         (good (beancount-ts-refile-test--file-buffer "good.beancount"))
         (failures (beancount-ts--save-buffers (list bad good))))
    (should (equal (mapcar #'car failures) (list bad)))
    (should-not (buffer-modified-p good))))

(ert-deftest beancount-ts-refile/save-buffers-clean-run-reports-nothing ()
  "With every save succeeding, no failures are reported."
  (let ((good (beancount-ts-refile-test--file-buffer "good.beancount")))
    (should-not (beancount-ts--save-buffers (list good)))
    (should-not (buffer-modified-p good))))

(ert-deftest beancount-ts-refile/commit-leaves-source-unsaved-when-target-fails ()
  "A target that cannot reach disk aborts before the source is written.
The source holds the only copy of the entries until a target is saved,
so the refile must fail towards a duplicate, never towards a loss."
  (let ((bad (beancount-ts-refile-test--file-buffer "bad.beancount" t))
        (source (beancount-ts-refile-test--file-buffer "main.beancount")))
    (with-current-buffer source (save-buffer) (insert "trailing edit\n"))
    (should-error (beancount-ts--commit-refile source (list bad))
                  :type 'user-error)
    (should (buffer-modified-p source))
    (should-not (string-match-p
                 "trailing edit"
                 (with-temp-buffer
                   (insert-file-contents (buffer-file-name source))
                   (buffer-string))))))

(ert-deftest beancount-ts-refile/commit-saves-source-last ()
  "With every target saved, the source is written too."
  (let ((target (beancount-ts-refile-test--file-buffer "target.beancount"))
        (source (beancount-ts-refile-test--file-buffer "main.beancount")))
    (beancount-ts--commit-refile source (list target))
    (should-not (buffer-modified-p source))
    (should-not (buffer-modified-p target))))

;;; Entry collection and inference (tree-sitter)

;; Batch Emacs sees a bare `user-emacs-directory', so it never finds the
;; grammar Doom installed under its own cache dir.  Mirrors the setup in
;; `beancount-ts-mode-test.el'; the tests below skip themselves when the
;; grammar is missing.
(let ((dir (expand-file-name "~/.emacs.d/.local/cache/tree-sitter")))
  (when (file-directory-p dir)
    (add-to-list 'treesit-extra-load-path dir)))

(defconst beancount-ts-refile-test--ledger
  (concat "2024-01-01 open Liabilities:CapitalOne:QuicksilverVisa\n"
          "\n"
          "2024-01-02 * \"SAFEWAY\" \"SAFEWAY\"\n"
          "  Expenses:Food:Groceries\n"
          "  Liabilities:CapitalOne:QuicksilverVisa   -25.38 USD\n"
          "\n"
          "2024-01-03 balance Assets:Chase:Checking   24363.91 USD\n"
          "\n"
          "2024-01-04 price VASIX                     15.85 USD\n")
  "A ledger holding each entry kind the refiler must classify.")

(defmacro beancount-ts-refile-test--in-ledger (&rest body)
  "Run BODY in a `beancount-ts-mode' buffer holding the sample ledger."
  (declare (indent 0))
  `(progn
     (skip-unless (treesit-ready-p 'beancount t))
     (with-temp-buffer
       (insert beancount-ts-refile-test--ledger)
       (beancount-ts-mode)
       (goto-char (point-min))
       ,@body)))

(defun beancount-ts-refile-test--entry-at (pattern)
  "Return the refileable entry node containing PATTERN in the buffer."
  (goto-char (point-min))
  (search-forward pattern)
  (treesit-parent-until
   (treesit-node-at (point))
   (lambda (n) (member (treesit-node-type n)
                       beancount-ts--refileable-entry-types))))

(ert-deftest beancount-ts-refile/collect-includes-balance-and-price ()
  "Buffer collection picks up balance and price directives, not just txns."
  (beancount-ts-refile-test--in-ledger
    (should (equal (mapcar #'treesit-node-type
                           (beancount-ts--collect-entries-in-region
                            (point-min) (point-max)))
                   '("transaction" "balance" "price")))))

(ert-deftest beancount-ts-refile/relevant-accounts-honour-ignored-prefixes ()
  "Every prefix in the ignore list is skipped, and only those."
  (beancount-ts-refile-test--in-ledger
    (let ((entry (beancount-ts-refile-test--entry-at "SAFEWAY"))
          (beancount-ts-refile-ignored-account-prefixes '("Liabilities:")))
      (should (equal (beancount-ts--relevant-accounts entry)
                     '("Expenses:Food:Groceries"))))))

(ert-deftest beancount-ts-refile/relevant-accounts-default-skips-expenses ()
  "The default ignore list drops Expenses, so the card decides the target."
  (beancount-ts-refile-test--in-ledger
    (let ((entry (beancount-ts-refile-test--entry-at "SAFEWAY")))
      (should (equal (beancount-ts--relevant-accounts entry)
                     '("Liabilities:CapitalOne:QuicksilverVisa"))))))

(ert-deftest beancount-ts-refile/primary-account-follows-the-ignore-list ()
  "The prompt default comes from the same accounts inference uses.
Narrow the ignore list and an expense account can be primary; there
is no second, hardcoded allowlist of account roots."
  (beancount-ts-refile-test--in-ledger
    (let ((entry (beancount-ts-refile-test--entry-at "SAFEWAY"))
          (beancount-ts-refile-ignored-account-prefixes '("Liabilities:")))
      (should (equal (beancount-ts--primary-account entry)
                     "Expenses:Food:Groceries")))))

(ert-deftest beancount-ts-refile/relevant-accounts-of-balance ()
  "A balance directive contributes its own account, not a posting's."
  (beancount-ts-refile-test--in-ledger
    (should (equal (beancount-ts--relevant-accounts
                    (beancount-ts-refile-test--entry-at "balance Assets:Chase"))
                   '("Assets:Chase:Checking")))))

(ert-deftest beancount-ts-refile/infer-target-of-balance ()
  "A balance directive files to its account's journal file."
  (beancount-ts-refile-test--in-ledger
    (should (equal (beancount-ts--infer-target-file
                    (beancount-ts-refile-test--entry-at "balance Assets:Chase")
                    beancount-ts-refile-test--journal-files)
                   "assets/chase/checking.beancount"))))

(ert-deftest beancount-ts-refile/infer-target-of-price ()
  "A price directive carries no account, so it files to the prices file."
  (beancount-ts-refile-test--in-ledger
    (should (equal (beancount-ts--infer-target-file
                    (beancount-ts-refile-test--entry-at "price VASIX")
                    beancount-ts-refile-test--journal-files)
                   "prices.beancount"))))

(ert-deftest beancount-ts-refile/infer-target-of-price-absent-file ()
  "Without a prices file in the journal, a price stays put."
  (beancount-ts-refile-test--in-ledger
    (should-not (beancount-ts--infer-target-file
                 (beancount-ts-refile-test--entry-at "price VASIX")
                 '("assets/chase/checking.beancount")))))

(ert-deftest beancount-ts-refile/infer-target-of-transaction ()
  "Transactions still infer from their non-expense postings."
  (beancount-ts-refile-test--in-ledger
    (should (equal (beancount-ts--infer-target-file
                    (beancount-ts-refile-test--entry-at "SAFEWAY")
                    beancount-ts-refile-test--journal-files)
                   "liabilities/capitalone/quicksilver-visa.beancount"))))

(ert-deftest beancount-ts-refile/collect-adjacent-entries ()
  "Entries packed on consecutive lines are all collected.
Transactions are always blank-line separated, but balance and price
directives are routinely written back-to-back."
  (skip-unless (treesit-ready-p 'beancount t))
  (with-temp-buffer
    (insert "2024-01-03 balance Assets:Chase:Checking          1.00 USD\n"
            "2024-01-03 balance Assets:Chase:Checking          2.00 USD\n"
            "2024-01-04 price VASIX                           15.85 USD\n")
    (beancount-ts-mode)
    (should (equal (mapcar #'treesit-node-type
                           (beancount-ts--collect-entries-in-region
                            (point-min) (point-max)))
                   '("balance" "balance" "price")))))

(ert-deftest beancount-ts-refile/collect-excludes-open-directive ()
  "Account lifecycle directives are not refiled; they live in accounts.beancount."
  (beancount-ts-refile-test--in-ledger
    (should-not (member "open"
                        (mapcar #'treesit-node-type
                                (beancount-ts--collect-entries-in-region
                                 (point-min) (point-max)))))))

;;; Journal root

(ert-deftest beancount-ts-refile/journal-root-from-journal-file ()
  "The journal root is the directory of `beancount-ts-journal-file'."
  (let ((beancount-ts-journal-file "~/accounting/journal/main.beancount"))
    (should (equal (expand-file-name "~/accounting/journal/")
                   (beancount-ts--journal-root)))))

(ert-deftest beancount-ts-refile/journal-root-unset-errors ()
  "Without a journal file there is nowhere to refile to."
  (let ((beancount-ts-journal-file nil))
    (should-error (beancount-ts--journal-root) :type 'user-error)))


;;; Refiling end to end

(defmacro beancount-ts-refile-test--with-journal (&rest body)
  "Run BODY in a source buffer under a throwaway journal.
The journal has `assets/cash.beancount' as the only per-account file
and `inbox.beancount' as the source, visited in the current buffer.
`beancount-ts-journal-file' points into the journal for the duration."
  (declare (indent 0))
  `(progn
     (skip-unless (treesit-ready-p 'beancount t))
     (let* ((root (file-name-as-directory (make-temp-file "beancount-ts-journal" t)))
            (target (expand-file-name "assets/cash.beancount" root))
            (source (expand-file-name "inbox.beancount" root))
            (beancount-ts-journal-file (expand-file-name "main.beancount" root)))
       (make-directory (file-name-directory target) t)
       (with-temp-file target (insert "2024-01-01 open Assets:Cash\n"))
       (with-temp-file source
         (insert "; inbox\n"
                 "2024-01-02 * \"x\"\n  Assets:Cash  1 USD\n  Expenses:X\n"
                 "\n"
                 "2024-01-03 * \"y\"\n  Assets:Cash  2 USD\n  Expenses:Y\n"))
       (unwind-protect
           (with-current-buffer (find-file-noselect source)
             (beancount-ts-mode)
             (buffer-enable-undo)
             ,@body)
         (dolist (f (list source target))
           (when-let* ((b (get-file-buffer f)))
             (with-current-buffer b (set-buffer-modified-p nil))
             (kill-buffer b)))
         (delete-directory root t)))))

(defun beancount-ts-refile-test--target-text ()
  "Return the text of the throwaway journal's target buffer."
  (with-current-buffer (find-file-noselect
                        (expand-file-name "assets/cash.beancount"
                                          (file-name-directory beancount-ts-journal-file)))
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest beancount-ts-refile/buffer-moves-entries-and-keeps-rest ()
  "Refiling the buffer moves both transactions and leaves the comment."
  (beancount-ts-refile-test--with-journal
    (beancount-ts-refile-buffer)
    (should (equal "; inbox" (string-trim (buffer-string))))
    (should (string-match-p "\"x\"" (beancount-ts-refile-test--target-text)))
    (should (string-match-p "\"y\"" (beancount-ts-refile-test--target-text)))))

(ert-deftest beancount-ts-refile/buffer-then-undo-restores-both-sides ()
  "One undo in the source puts the entries back and clears the target."
  (beancount-ts-refile-test--with-journal
    (let ((before (buffer-string)))
      (beancount-ts-refile-buffer)
      (undo)
      (should (equal before (buffer-string)))
      (should-not (string-match-p "\"x\"" (beancount-ts-refile-test--target-text))))))

(ert-deftest beancount-ts-refile/single-then-undo-restores-both-sides ()
  "The same holds for a single inferred refile."
  (beancount-ts-refile-test--with-journal
    (let ((before (buffer-string)))
      (search-forward "\"y\"")
      (beancount-ts-refile-transaction)
      (should-not (string-match-p "\"y\"" (buffer-string)))
      (should (string-match-p "\"x\"" (buffer-string)))
      (should (string-match-p "\"y\"" (beancount-ts-refile-test--target-text)))
      (undo)
      (should (equal before (buffer-string)))
      (should-not (string-match-p "\"y\"" (beancount-ts-refile-test--target-text))))))


(defun beancount-ts-refile-test--undo ()
  "Undo one step the way the command loop would, starting a fresh chain.
The command loop closes each command with a boundary; batch code must."
  (let ((last-command 'ignore))
    (undo)
    (undo-boundary)))

(defun beancount-ts-refile-test--undo-more ()
  "Undo the next step of a chain the previous `undo' began."
  (let ((last-command 'undo))
    (undo)
    (undo-boundary)))

(ert-deftest beancount-ts-refile/two-refiles-into-one-target-undo-twice ()
  "Two refiles into the same file undo one after the other, no recursion.
Undoing the second must not re-run the first's undo entry, which used
to recurse until the Lisp stack overflowed."
  (beancount-ts-refile-test--with-journal
    (let ((before (buffer-string)))
      (search-forward "\"y\"")
      (beancount-ts-refile-transaction)
      (goto-char (point-min))
      (search-forward "\"x\"")
      (beancount-ts-refile-transaction)
      (beancount-ts-refile-test--undo)
      (should (string-match-p "\"x\"" (buffer-string)))
      (should-not (string-match-p "\"x\"" (beancount-ts-refile-test--target-text)))
      (should (string-match-p "\"y\"" (beancount-ts-refile-test--target-text)))
      (beancount-ts-refile-test--undo-more)
      (should (equal before (buffer-string)))
      (should-not (string-match-p "\"y\"" (beancount-ts-refile-test--target-text))))))

(ert-deftest beancount-ts-refile/undo-removes-only-the-refiled-text ()
  "A later edit in the target survives an undo of the refile."
  (beancount-ts-refile-test--with-journal
    (let ((before (buffer-string)))
      (beancount-ts-refile-buffer)
      (with-current-buffer (find-file-noselect
                            (expand-file-name "assets/cash.beancount"
                                              (file-name-directory beancount-ts-journal-file)))
        (goto-char (point-max))
        (insert "; a note typed after the refile\n"))
      (beancount-ts-refile-test--undo)
      (should (equal before (buffer-string)))
      (let ((target (beancount-ts-refile-test--target-text)))
        (should-not (string-match-p "\"x\"" target))
        (should (string-match-p "a note typed after" target))))))

(ert-deftest beancount-ts-refile/redo-puts-entries-back-in-the-target ()
  "Redoing an undone refile re-appends to the target, so the entries
never exist in no buffer at all."
  (beancount-ts-refile-test--with-journal
    (beancount-ts-refile-buffer)
    (let ((refiled-source (buffer-string))
          (refiled-target (beancount-ts-refile-test--target-text)))
      (beancount-ts-refile-test--undo)
      ;; A second fresh `undo' after a non-undo command is the redo.
      (beancount-ts-refile-test--undo)
      (should (equal refiled-source (buffer-string)))
      (should (equal refiled-target (beancount-ts-refile-test--target-text))))))

(ert-deftest beancount-ts-refile/undo-refuses-when-target-was-killed ()
  "With the target buffer gone, undo refuses rather than duplicating.
Restoring the source while the target file keeps the entries would put
them in two places on disk."
  (beancount-ts-refile-test--with-journal
    (beancount-ts-refile-buffer)
    (let ((after (buffer-string)))
      (kill-buffer (find-file-noselect
                    (expand-file-name "assets/cash.beancount"
                                      (file-name-directory beancount-ts-journal-file))))
      (should-error (beancount-ts-refile-test--undo) :type 'user-error)
      (should (equal after (buffer-string))))))

(ert-deftest beancount-ts-refile/undo-last-refile-survives-a-source-edit ()
  "`beancount-ts-undo-last-refile' restores the entries and clears the
target even after the source was edited, keeping that edit."
  (beancount-ts-refile-test--with-journal
    (beancount-ts-refile-buffer)
    (goto-char (point-max))
    (insert "; typed after the refile\n")
    (beancount-ts-undo-last-refile)
    (should (string-match-p "\"x\"" (buffer-string)))
    (should (string-match-p "\"y\"" (buffer-string)))
    (should (string-match-p "typed after the refile" (buffer-string)))
    (should-not (string-match-p "\"x\"" (beancount-ts-refile-test--target-text)))))

(ert-deftest beancount-ts-refile/undo-last-refile-after-undo-is-refused ()
  "Once the refile was undone by plain undo there is nothing left to undo."
  (beancount-ts-refile-test--with-journal
    (beancount-ts-refile-buffer)
    (beancount-ts-refile-test--undo)
    (should-error (beancount-ts-undo-last-refile) :type 'user-error)))


;;; Source detection

(ert-deftest beancount-ts-refile/source-p-sees-through-symlinks ()
  "A journal root reached through a symlink still recognises its own files.
Doom sets `find-file-visit-truename', so `buffer-file-name' is the real
path while `beancount-ts-journal-file' may go through the link."
  (let* ((real (file-name-as-directory (make-temp-file "beancount-ts-real" t)))
         (link (concat (make-temp-file "beancount-ts-link") "-dir"))
         (file (expand-file-name "inbox.beancount" real)))
    (make-symbolic-link real link)
    (with-temp-file file (insert ""))
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (should (beancount-ts--source-p "inbox.beancount"
                                          (file-name-as-directory link)))
          (kill-buffer))
      (delete-file link)
      (delete-directory real t))))

;;; Prompting

(ert-deftest beancount-ts-refile/empty-prompt-answer-refiles-nothing ()
  "RET on an empty prompt is a refusal, not a refile into the journal root."
  (beancount-ts-refile-test--with-journal
    (let ((before (buffer-string)))
      (search-forward "\"y\"")
      (cl-letf (((symbol-function 'beancount-ts--infer-target-file)
                 (lambda (&rest _) nil))
                ((symbol-function 'completing-read)
                 (lambda (&rest _) "")))
        (should-error (beancount-ts-refile-transaction) :type 'user-error))
      (should (equal before (buffer-string)))
      (should-not (buffer-modified-p)))))

(ert-deftest beancount-ts-refile/prompt-default-prefers-the-ancestor-file ()
  "With a parent file and a sibling's file both present, the default is
the parent, as inference would choose, not the sibling that happens to
share the longest prefix."
  (should (equal (beancount-ts--default-file-for
                  "liabilities/chase/ink-visa"
                  '("liabilities/chase/sapphire.beancount"
                    "liabilities/chase.beancount"))
                 "liabilities/chase.beancount")))

(ert-deftest beancount-ts-refile/prompt-default-falls-back-to-fuzzy-match ()
  "Without an ancestor file the closest spelling still serves as default."
  (should (equal (beancount-ts--default-file-for
                  "liabilities/chase/ink-visa"
                  '("liabilities/chase/sapphire.beancount"))
                 "liabilities/chase/sapphire.beancount")))

(provide 'beancount-ts-refile-test)
;;; beancount-ts-refile-test.el ends here
