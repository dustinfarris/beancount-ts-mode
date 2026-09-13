# beancount-ts-mode

A major mode for editing [Beancount](https://beancount.github.io/) files, built on Emacs's own tree-sitter support (`treesit`). Extracted from [dustinfarris/dotfiles](https://github.com/dustinfarris/dotfiles), where it grew alongside a Doom Emacs module.

## Features

- Tree-sitter syntax highlighting. Accounts are fontified per component: root, sub-accounts and the colons between them each get their own face.
- Entries (dated directives and the undated top-level ones) are tree-sitter `defun` things and postings are `sentence` things, so `C-M-a`, `C-M-e`, `C-M-h`, `narrow-to-defun` and `M-e` work, including for entries nested under org-style `*` headings.
- Imenu (sections, opened accounts, transactions by date, payee and narration), `which-function-mode`, and `outline-minor-mode` levelled by the `*` headings.
- Posting indentation on `RET` and `TAB`.
- Commands to walk transactions (optionally only uncleared ones), mark them cleared or pending, clone them, and sort entries by date without losing comments, blank lines or headings.
- An eglot entry for [beancount-language-server](https://github.com/polarmutex/beancount-language-server).
- `beancount-ts-refile`: file entries from an inbox ledger into per-account journal files, inferring the target from the accounts each entry names, with one undo step across every buffer touched.
- `beancount-mode` is declared a parent with `derived-mode-add-parents`, so yasnippet tables, eglot entries and anything else keyed on `beancount-mode` apply here too.

## Requirements

- Emacs 31.1 or later, built with tree-sitter.
- The [tree-sitter-beancount](https://github.com/polarmutex/tree-sitter-beancount) grammar. The first `.beancount` file you open offers to build it from the source pinned in `beancount-ts-mode.el` (see `treesit-auto-install-grammar`); to build it by hand, `M-x treesit-install-language-grammar RET beancount RET`. Building needs `git` and a C compiler.
- Optionally, `beancount-language-server` on `PATH`.

## Installation

Not on MELPA yet. With the built-in `package-vc`:

```elisp
(package-vc-install "https://github.com/dustinfarris/beancount-ts-mode")
```

With Doom Emacs, in `packages.el`:

```elisp
(package! beancount-ts-mode
  :recipe (:host github :repo "dustinfarris/beancount-ts-mode" :files ("*.el")))
```

Loading `beancount-ts-mode` puts `.beancount` and `.bean` files into the mode. The refile commands are autoloaded from `beancount-ts-refile`.

## Configuration

```elisp
;; Where beancount-language-server and the refiler find the journal.
(setq beancount-ts-journal-file "~/accounting/journal/main.beancount")

;; Flags the server's own flagged-entry scan warns on (default ("!")).
(setq beancount-ts-diagnostic-flags '("!"))

;; Columns postings indent to (default 2).
(setq beancount-ts-indent-offset 2)

;; Refiling: second account components that name a business entity and
;; lead the path (default nil), and account prefixes that never decide a
;; target (default Expenses and Equity).
(setq beancount-ts-refile-entities '("Consulting")
      beancount-ts-refile-ignored-account-prefixes '("Expenses:" "Equity:" "Assets:Transfer:")
      beancount-ts-prices-file "prices.beancount")
```

The mode binds no keys of its own. A starting point:

```elisp
(with-eval-after-load 'beancount-ts-mode
  (define-key beancount-ts-mode-map (kbd "C-c C-c") #'beancount-ts-transaction-clear)
  (define-key beancount-ts-mode-map (kbd "C-M-n") #'beancount-ts-next-transaction)
  (define-key beancount-ts-mode-map (kbd "C-M-p") #'beancount-ts-prev-transaction)
  (define-key beancount-ts-mode-map (kbd "C-c C-r") #'beancount-ts-refile-transaction)
  (define-key beancount-ts-mode-map (kbd "C-c C-s") #'beancount-ts-sort))
```

### LSP

Loading the mode adds a `beancount-ts-mode` entry to `eglot-server-programs` that starts `beancount-language-server --stdio` and hands it `beancount-ts-journal-file` as `journal_file` and `beancount-ts-diagnostic-flags` as `diagnostic_flags`. Start it with `M-x eglot`.

The server's bean-check pass also warns on `!`, and the two lists are appended rather than merged ([polarmutex/beancount-language-server#825](https://github.com/polarmutex/beancount-language-server/issues/825)); set `beancount-ts-diagnostic-flags` to a different flag, or to `nil`, if you see each pending transaction reported twice.

## Commands

| Command                                                | Description                                                                                                                |
|--------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------|
| `beancount-ts-next-transaction`                        | Next transaction, skipping other directives; takes a count                                                                 |
| `beancount-ts-prev-transaction`                        | Previous transaction                                                                                                       |
| `beancount-ts-next-uncleared-transaction`              | Next transaction not marked `*`                                                                                            |
| `beancount-ts-transaction-clear`                       | Mark the transaction at point, or every one in the region, `*`; with a prefix argument `!`                                 |
| `beancount-ts-transaction-clear-and-next`              | Mark cleared, then jump to the next uncleared transaction                                                                  |
| `beancount-ts-clone-transaction`                       | Insert a copy of the transaction at point below it                                                                         |
| `beancount-ts-sort`                                    | Sort entries by date: the region if active, else the whole buffer                                                          |
| `beancount-ts-sort-buffer`, `beancount-ts-sort-region` | The two halves of the above                                                                                                |
| `beancount-ts-refile-transaction`                      | Refile the entry at point, prompting when the target is ambiguous; with a region, refile every entry whose target is clear |
| `beancount-ts-refile-buffer`                           | Refile every entry in the buffer whose target is clear                                                                     |
| `beancount-ts-undo-last-refile`                        | Put the last refile's entries back in the source and out of every target, keeping edits made since                         |

Entry motion is the stock defun vocabulary: `C-M-a`, `C-M-e`, `C-M-h`, `narrow-to-defun`. Postings are sentences, so `M-e` steps through them. `beancount-ts-bounds-of-transaction` and `beancount-ts-inner-of-transaction` return the bounds of the transaction at point and of its postings, for building text objects in Meow, evil or the like.

Sorting swaps entries between their slots and leaves comments, blank lines and headings where they are, so nothing is dropped; an entry can move across a heading. Undated entries (`option`, `include`, ...) hold their slot.

### How refiling picks a file

Transactions, `balance` and `price` directives are refileable; `open`, `close` and `commodity` are not. Price directives go to `beancount-ts-prices-file`. For the others, each account the entry names is turned into a path guess (`Liabilities:CapitalOne:QuicksilverVisa` becomes `liabilities/capital-one/quicksilver-visa`), then matched against the `.beancount` files under the journal directory, walking up the path until a file exists. Matching ignores hyphens, so `capitalone/` and `capital-one/` both resolve. If every relevant account agrees on one file, the entry goes there; otherwise you are prompted with the best guess as the default. Accounts matching `beancount-ts-refile-ignored-account-prefixes` never take part. A second component listed in `beancount-ts-refile-entities` leads the path, so `Liabilities:Consulting:Chase:InkVisa` resolves under `consulting/liabilities/chase/`.

Target files are saved before the source, so an interrupted refile fails towards a duplicate rather than a loss.

## Faces

| Face                             | Inherits                       | Used for                    |
|----------------------------------|--------------------------------|-----------------------------|
| `beancount-ts-date`              | `font-lock-number-face`        | Dates                       |
| `beancount-ts-account`           | `font-lock-builtin-face`       | Root account component      |
| `beancount-ts-account-sub`       | `default`                      | Sub-account components      |
| `beancount-ts-account-separator` | `font-lock-comment-face`       | Colons between components   |
| `beancount-ts-amount`            | `font-lock-number-face`        | Numbers                     |
| `beancount-ts-currency`          | `font-lock-type-face`          | Currencies                  |
| `beancount-ts-directive`         | `font-lock-keyword-face`       | Directive keywords          |
| `beancount-ts-tag`               | `font-lock-preprocessor-face`  | Tags                        |
| `beancount-ts-link`              | `font-lock-preprocessor-face`  | Links                       |
| `beancount-ts-string`            | `font-lock-string-face`        | Strings, payees, narrations |
| `beancount-ts-metadata-key`      | `font-lock-property-name-face` | Metadata keys               |
| `beancount-ts-flag-pending`      | `font-lock-warning-face`       | `!` and other flags         |
| `beancount-ts-flag-cleared`      | `success`                      | `*`                         |

## Development

```bash
make grammar   # build the pinned tree-sitter grammar into ~/.emacs.d/tree-sitter
make test      # ERT, in batch
make lint      # byte-compile with warnings as errors
```

Tests that need the grammar skip themselves when it is missing.

## License

GPL-3.0-or-later.
