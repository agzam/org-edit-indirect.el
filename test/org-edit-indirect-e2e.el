;;; org-edit-indirect-e2e.el --- Keystroke E2E of org-edit-indirect -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;  Visits each case's Org text as a file in batch Emacs and types the
;;  case's keys with `execute-kbd-macro'.  Then it compares the Org text,
;;  the selected buffer, the edit buffers left open and the error with
;;  what the case expects.  `make test-e2e' runs it.
;;
;;; Code:

(require 'cl-lib)
(require 'org)
(require 'org-src)
(require 'edit-indirect)
(require 'org-edit-indirect)

;;; Emacs as a person starts it

(add-hook 'org-mode-hook #'org-edit-indirect-mode)
;; Batch Emacs starts with it off, and DEL deletes the region only with it on.
(transient-mark-mode 1)

;;; Cases

(defconst org-edit-indirect-e2e--cases
  '(("types a body under an empty heading"
     :text "* Journal\n** My dear, sweet daughter\n** Next day\n"
     :at "sweet"
     :keys "C-c ' Dear SPC diary. RET C-c C-c"
     :expect "* Journal\n** My dear, sweet daughter\nDear diary.\n** Next day\n")

    ("types a body under the last heading of a file with no final newline"
     :text "* Journal\n** Today"
     :at "Today"
     :keys "C-c ' RET Wrote SPC tests. C-c C-c"
     :expect "* Journal\n** Today\nWrote tests.\n")

    ("adds to the body of a heading"
     :text "* Groceries\nmilk\n* Chores\n"
     :at "Groceries"
     :keys "C-c ' eggs RET C-c C-c"
     :expect "* Groceries\nmilk\neggs\n* Chores\n")

    ("edits the body of a heading from a link in its title"
     :text "* Read [[https://orgmode.org][the manual]]\nchapter one\n* Other\nmore\n"
     :at "the manual"
     :keys "C-c ' chapter SPC two RET C-c C-c"
     :expect "* Read [[https://orgmode.org][the manual]]\nchapter one\nchapter two\n* Other\nmore\n")

    ("adds to a paragraph"
     :text "* Notes\nfirst line\n\nnext paragraph\n"
     :at "first"
     :keys "C-c ' second SPC line RET C-c C-c"
     :expect "* Notes\nfirst line\nsecond line\n\nnext paragraph\n")

    ("edits the paragraph from a link in it"
     :text "* Notes\nsee [[https://orgmode.org][Org]] for more\n\nnext paragraph\n"
     :at "Org]]"
     :keys "C-c ' Thanks. RET C-c C-c"
     :expect "* Notes\nsee [[https://orgmode.org][Org]] for more\nThanks.\n\nnext paragraph\n")

    ("edits the paragraph from bold text in it"
     :text "* Notes\nsome *bold* claim\n\nnext paragraph\n"
     :at "bold"
     :keys "C-c ' Proof. RET C-c C-c"
     :expect "* Notes\nsome *bold* claim\nProof.\n\nnext paragraph\n")

    ("edits the whole quote block from its second paragraph"
     :text "#+begin_quote\nfirst paragraph\n\nsecond paragraph\n#+end_quote\n"
     :at "second"
     :keys "C-c ' M-< Epigraph. RET C-c C-c"
     :expect "#+begin_quote\nEpigraph.\nfirst paragraph\n\nsecond paragraph\n#+end_quote\n")

    ("ends a quote block's last line, typed without a newline"
     :text "#+begin_quote\nold words\n#+end_quote\n"
     :at "old"
     :keys "C-c ' C-x h DEL new SPC words C-c C-c"
     :expect "#+begin_quote\nnew words\n#+end_quote\n")

    ("clears a quote block"
     :text "#+begin_quote\nold words\n#+end_quote\n"
     :at "old"
     :keys "C-c ' C-x h DEL C-c C-c"
     :expect "#+begin_quote\n#+end_quote\n")

    ("fills an empty quote block"
     :text "#+begin_quote\n#+end_quote\n"
     :at "begin"
     :keys "C-c ' M-< C-e RET To SPC be. C-c C-c"
     :expect "#+begin_quote\nTo be.\n#+end_quote\n")

    ("adds to a special block"
     :text "#+begin_note\nRemember the milk.\n#+end_note\n"
     :at "begin_note"
     :keys "C-c ' And SPC the SPC eggs. RET C-c C-c"
     :expect "#+begin_note\nRemember the milk.\nAnd the eggs.\n#+end_note\n")

    ("edits the whole drawer from its second note"
     :text "* Task\n:NOTES:\nfirst note\n\nsecond note\n:END:\nbody\n"
     :at "second"
     :keys "C-c ' M-< Summary. RET C-c C-c"
     :expect "* Task\n:NOTES:\nSummary.\nfirst note\n\nsecond note\n:END:\nbody\n")

    ("adds a property from the line that opens the property drawer"
     :text "* Task\n:PROPERTIES:\n:ID: 42\n:END:\nbody\n"
     :at "PROPERTIES"
     :keys "C-c ' :EFFORT: SPC 1:00 RET C-c C-c"
     :expect "* Task\n:PROPERTIES:\n:ID: 42\n:EFFORT: 1:00\n:END:\nbody\n")

    ("adds an item to the whole list from its first item"
     :text "* Shopping\n- milk\n- bread\n\nafter the list\n"
     :at "milk"
     :keys "C-c ' - SPC eggs RET C-c C-c"
     :expect "* Shopping\n- milk\n- bread\n- eggs\n\nafter the list\n")

    ("edits a comment block in Org's own editor"
     :text "#+begin_comment\nprivate thought\n#+end_comment\n"
     :at "private"
     :keys "C-c ' M-> * SPC not SPC a SPC heading RET C-c '"
     :expect "#+begin_comment\nprivate thought\n,* not a heading\n#+end_comment\n")

    ("edits a source block in Org's own editor"
     :text "#+begin_src emacs-lisp\n(message \"hi\")\n#+end_src\n"
     :at "message"
     :keys "C-c ' M-> (+ SPC 1 SPC 2) C-c '"
     :bindings ((org-src-preserve-indentation . t))
     :expect "#+begin_src emacs-lisp\n(message \"hi\")\n(+ 1 2)\n#+end_src\n")

    ("saves, then commits, under an empty heading"
     :text "* Journal\n** Day one\n** Day two\n"
     :at "Day one"
     :keys "C-c ' first RET C-x C-s second RET C-c C-c"
     :expect "* Journal\n** Day one\nfirst\nsecond\n** Day two\n")

    ("aborts without a change"
     :text "* Groceries\nmilk\n* Chores\n"
     :at "Groceries"
     :keys "C-c ' eggs RET C-c C-k"
     :expect "* Groceries\nmilk\n* Chores\n")

    ("refuses a read-only buffer before it opens an edit buffer"
     :text "* Groceries\nmilk\n"
     :at "Groceries"
     :keys "C-x C-q C-c ' eggs RET C-c C-c"
     :expect "* Groceries\nmilk\n"
     :error "Buffer is read-only: #<buffer notes.org>")

    ("returns to the open edit buffer of the same heading"
     :text "* Groceries\nmilk\n* Chores\n"
     :at "Groceries"
     :keys "C-c ' C-x o C-c ' eggs RET C-c C-c"
     :expect "* Groceries\nmilk\neggs\n* Chores\n"))
  "The cases, each (NAME :text :at :keys :expect [:error] [:bindings]).
KEYS, in the notation of `kbd', run from the first AT in the Org file
holding TEXT, with BINDINGS, an alist, in effect.  EXPECT is the Org
text after them, ERROR the message of the error they end in.")

;;; Running a case

(defconst org-edit-indirect-e2e--file
  (expand-file-name "notes.org" (make-temp-file "org-edit-indirect-e2e-" t))
  "The Org file the cases type into.")

(defun org-edit-indirect-e2e--edit-buffer-p (buffer)
  "Return non-nil when BUFFER is an edit buffer of edit-indirect or Org."
  (or (edit-indirect-buffer-indirect-p buffer)
      (with-current-buffer buffer
        (org-src-edit-buffer-p))))

(defun org-edit-indirect-e2e--reset ()
  "Show *scratch* alone and kill the buffers of the last case."
  (delete-other-windows)
  (switch-to-buffer "*scratch*")
  (let ((kill-buffer-query-functions nil))
    (thread-last
      (buffer-list)
      (seq-remove (lambda (buffer)
                    (or (string-prefix-p " " (buffer-name buffer))
                        (member (buffer-name buffer)
                                '("*scratch*" "*Messages*")))))
      (mapc (lambda (buffer)
              (with-current-buffer buffer
                (set-buffer-modified-p nil))
              (kill-buffer buffer))))))

(defun org-edit-indirect-e2e--type (keys bindings)
  "Type KEYS with BINDINGS in effect; return the message of their error."
  (cl-progv (mapcar #'car bindings) (mapcar #'cdr bindings)
    (condition-case err
        (let ((inhibit-message t))
          (execute-kbd-macro (kbd keys))
          nil)
      ((error quit) (error-message-string err)))))

(defun org-edit-indirect-e2e--run (case)
  "Type the keys of CASE into its Org file and return where they end."
  (cl-destructuring-bind (_name &key text at keys bindings &allow-other-keys)
      case
    (org-edit-indirect-e2e--reset)
    (with-temp-file org-edit-indirect-e2e--file
      (insert text))
    (find-file org-edit-indirect-e2e--file)
    (font-lock-ensure)
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (search-forward at))
    (goto-char (match-beginning 0))
    (let* ((org (current-buffer))
           (failure (org-edit-indirect-e2e--type keys bindings)))
      (list :text (with-current-buffer org
                    (buffer-substring-no-properties (point-min) (point-max)))
            :buffer (thread-last (selected-window) window-buffer buffer-name)
            :edit-buffers (thread-last (buffer-list)
                                       (seq-filter #'org-edit-indirect-e2e--edit-buffer-p)
                                       (mapcar #'buffer-name))
            :error failure))))

(defun org-edit-indirect-e2e--expected (case)
  "Return where the keys of CASE should end."
  (cl-destructuring-bind (_name &key expect ((:error failure)) &allow-other-keys)
      case
    (list :text expect
          :buffer (file-name-nondirectory org-edit-indirect-e2e--file)
          :edit-buffers nil
          :error failure)))

(defun org-edit-indirect-e2e--messages-since (mark)
  "Return what *Messages* got after position MARK."
  (with-current-buffer (messages-buffer)
    (buffer-substring-no-properties (min mark (point-max)) (point-max))))

(defun org-edit-indirect-e2e--check (case)
  "Run CASE, print its verdict, and return non-nil when it passed."
  (let* ((mark (with-current-buffer (messages-buffer) (point-max)))
         (state (org-edit-indirect-e2e--run case))
         (expected (org-edit-indirect-e2e--expected case))
         (print-escape-newlines t))
    (if (equal state expected)
        (princ (format "ok   %s\n" (car case)))
      (princ (format "FAIL %s\n  keys      %s\n  expected  %S\n  got       %S\n  messages  %S\n"
                     (car case) (plist-get (cdr case) :keys) expected state
                     (org-edit-indirect-e2e--messages-since mark))))
    (equal state expected)))

(defun org-edit-indirect-e2e-main ()
  "Run every case, print a verdict line for each and a summary, and exit."
  (princ (format "org-edit-indirect-e2e on Emacs %s, Org %s\n"
                 emacs-version (org-version)))
  (let* ((start (float-time))
         (failed (seq-count #'null (mapcar #'org-edit-indirect-e2e--check
                                           org-edit-indirect-e2e--cases)))
         (elapsed (- (float-time) start)))
    (princ (format "org-edit-indirect-e2e: %d cases, %d failed, %.1f s\n"
                   (length org-edit-indirect-e2e--cases) failed elapsed))
    (org-edit-indirect-e2e--reset)
    (delete-directory (file-name-directory org-edit-indirect-e2e--file) t)
    (kill-emacs (min failed 1))))

(org-edit-indirect-e2e-main)

;;; org-edit-indirect-e2e.el ends here
