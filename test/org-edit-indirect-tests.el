;;; org-edit-indirect-tests.el --- Tests for org-edit-indirect.el -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;;; Commentary:
;;  What `org-edit-indirect-special' edits at each point of an Org buffer,
;;  and how its edit buffer commits back.
;;
;;; Code:

(require 'buttercup)
(require 'org-edit-indirect)

;;; Helpers

(defmacro org-edit-indirect-test--with-org (text &rest body)
  "Run BODY in a displayed Org buffer holding TEXT.
Kill the buffer and every edit-indirect buffer afterwards."
  (declare (indent 1) (debug t))
  `(let ((org-buffer (generate-new-buffer "test.org")))
     (unwind-protect
         (progn
           (switch-to-buffer org-buffer)
           (org-mode)
           (insert ,text)
           ,@body)
       (mapc #'kill-buffer
             (seq-filter #'edit-indirect-buffer-indirect-p (buffer-list)))
       (kill-buffer org-buffer))))

(defun org-edit-indirect-test--goto (needle &optional offset)
  "Put point OFFSET characters after the start of the first NEEDLE."
  (goto-char (point-min))
  (search-forward needle)
  (goto-char (+ (match-beginning 0) (or offset 0))))

(defun org-edit-indirect-test--edited (text needle &optional offset)
  "Return what `org-edit-indirect-special' edits in TEXT at NEEDLE.
That is the text of the edit buffer, or the symbol `org' when the
command leaves the element to `org-edit-special'.  OFFSET moves point
past the start of NEEDLE."
  (org-edit-indirect-test--with-org text
    (org-edit-indirect-test--goto needle offset)
    (let ((result (org-edit-indirect-special)))
      (if (bufferp result)
          (with-current-buffer result (buffer-string))
        result))))

(defun org-edit-indirect-test--invisible ()
  "Return the positions of the current buffer that are invisible."
  (seq-filter #'invisible-p (number-sequence (point-min) (1- (point-max)))))

;;; What C-c ' edits

(describe "org-edit-indirect-special"
  (before-each
    (spy-on 'org-edit-special :and-return-value 'org))

  (describe "on a heading"
    (it "edits the body"
      (expect (org-edit-indirect-test--edited "* H\nbody\n* N\n" "H")
              :to-equal "body\n"))

    (it "edits the subheadings with the body"
      (expect (org-edit-indirect-test--edited
               "* H\nbody\n** S\nsub\n* N\n" "H")
              :to-equal "body\n** S\nsub\n"))

    (it "edits the planning line and the drawers with the body"
      (expect (org-edit-indirect-test--edited
               "* TODO H\nSCHEDULED: <2026-11-16 Mon>\n:PROPERTIES:\n:ID: x\n:END:\nbody\n"
               " H")
              :to-equal
              "SCHEDULED: <2026-11-16 Mon>\n:PROPERTIES:\n:ID: x\n:END:\nbody\n"))

    (it "edits the heading line when the heading has no body"
      (expect (org-edit-indirect-test--edited "* A\n** E\n* C\n" "E")
              :to-equal "** E\n"))

    (it "leaves the blank lines after an empty heading out"
      (expect (org-edit-indirect-test--edited "* A\n** E\n\n\n* C\n" "E")
              :to-equal "** E\n"))

    (it "edits an empty heading at the end of the buffer"
      (expect (org-edit-indirect-test--edited "* A\n** E" "E")
              :to-equal "** E"))

    (it "edits the body from a link in the title"
      (expect (org-edit-indirect-test--edited
               "intro\n* H [[https://x.org][lnk]]\nbody\n* N\nnext\n" "lnk")
              :to-equal "body\n"))

    (it "edits the body from the tags"
      (expect (org-edit-indirect-test--edited "* H   :tag:\nbody\n" "tag")
              :to-equal "body\n"))

    (it "leaves a timestamp in the title to Org"
      (expect (org-edit-indirect-test--edited
               "* H <2026-11-16 Mon>\nbody\n" "2026")
              :to-be 'org)))

  (describe "on a paragraph"
    (it "edits the paragraph"
      (expect (org-edit-indirect-test--edited
               "* H\nfirst para\n\nsecond para\n" "first")
              :to-equal "first para\n"))

    (it "edits the paragraph from a link in it"
      (expect (org-edit-indirect-test--edited
               "* H\nsee [[https://x.org][lnk]] here\n\nnext\n" "lnk")
              :to-equal "see [[https://x.org][lnk]] here\n"))

    (it "edits the paragraph from markup in it"
      (expect (org-edit-indirect-test--edited
               "* H\nsome *bold* text\n\nnext\n" "bold")
              :to-equal "some *bold* text\n"))

    (it "edits the paragraph above a blank line"
      (expect (org-edit-indirect-test--edited
               "* H\npara one\n\npara two\n" "one\n" 4)
              :to-equal "para one\n"))

    (it "edits the paragraph of a footnote definition"
      (expect (org-edit-indirect-test--edited
               "* H\ntext[fn:1]\n\n[fn:1] the note\n" "the note")
              :to-equal "the note\n"))

    (it "leaves objects with an Org editor to Org"
      (expect (org-edit-indirect-test--edited
               "* H\ntext[fn:1] more\n\n[fn:1] note\n" "fn:1")
              :to-be 'org)
      (expect (org-edit-indirect-test--edited
               "* H\nsum src_emacs-lisp{(+ 1 2)} here\n" "(+ 1")
              :to-be 'org)
      (expect (org-edit-indirect-test--edited "* H\nsee \\(x^2\\) here\n" "x^2")
              :to-be 'org)
      (expect (org-edit-indirect-test--edited
               "* H\nmeet <2026-11-16 Mon> there\n" "2026")
              :to-be 'org)))

  (describe "on a block"
    (it "edits a quote block from its first line"
      (expect (org-edit-indirect-test--edited
               "#+begin_quote\nfoo\n\nbar\n#+end_quote\n" "begin_quote")
              :to-equal "foo\n\nbar\n"))

    (it "edits the whole quote block from text inside it"
      (expect (org-edit-indirect-test--edited
               "#+begin_quote\nfoo\n\nbar\n#+end_quote\n" "bar")
              :to-equal "foo\n\nbar\n"))

    (it "edits a verse block"
      (expect (org-edit-indirect-test--edited
               "#+begin_verse\nfoo\nbar\n#+end_verse\n" "bar")
              :to-equal "foo\nbar\n"))

    (it "edits a special block from text inside it"
      (expect (org-edit-indirect-test--edited
               "#+begin_note\nfoo\n\nbar\n#+end_note\n" "bar")
              :to-equal "foo\n\nbar\n"))

    (it "edits a center block"
      (expect (org-edit-indirect-test--edited
               "#+begin_center\nfoo\n#+end_center\n" "begin_center")
              :to-equal "foo\n"))

    (it "edits an empty block with its delimiters"
      (expect (org-edit-indirect-test--edited
               "#+begin_quote\n#+end_quote\n" "begin_quote")
              :to-equal "#+begin_quote\n#+end_quote\n"))

    (it "leaves the keywords above an empty block out"
      (expect (org-edit-indirect-test--edited
               "#+name: q\n#+begin_quote\n#+end_quote\n" "begin_quote")
              :to-equal "#+begin_quote\n#+end_quote\n"))

    (it "leaves comment, source and example blocks to Org"
      (expect (org-edit-indirect-test--edited
               "#+begin_comment\nfoo\n#+end_comment\n" "foo")
              :to-be 'org)
      (expect (org-edit-indirect-test--edited
               "#+begin_src emacs-lisp\n(+ 1 2)\n#+end_src\n" "(+ 1")
              :to-be 'org)
      (expect (org-edit-indirect-test--edited
               "#+begin_example\nfoo\n#+end_example\n" "foo")
              :to-be 'org)))

  (describe "on a drawer"
    (it "edits a drawer from its first line"
      (expect (org-edit-indirect-test--edited
               "* H\n:NOTES:\ninside\n:END:\nbody\n" "NOTES")
              :to-equal "inside\n"))

    (it "edits the whole drawer from text inside it"
      (expect (org-edit-indirect-test--edited
               "* H\n:NOTES:\nfirst\n\nsecond\n:END:\nbody\n" "second")
              :to-equal "first\n\nsecond\n"))

    (it "edits an empty drawer with its delimiters"
      (expect (org-edit-indirect-test--edited
               "* H\n:NOTES:\n:END:\nbody\n" "NOTES")
              :to-equal ":NOTES:\n:END:\n"))

    (it "edits the properties of a property drawer"
      (expect (org-edit-indirect-test--edited
               "* H\n:PROPERTIES:\n:ID: x\n:END:\nbody\n" "PROPERTIES")
              :to-equal ":ID: x\n"))

    (it "edits the property drawer from a property line"
      (expect (org-edit-indirect-test--edited
               "* H\n:PROPERTIES:\n:ID: x\n:CUSTOM: y\n:END:\nbody\n" ":CUSTOM:")
              :to-equal ":ID: x\n:CUSTOM: y\n"))

    (it "leaves a clock line to Org"
      (expect (org-edit-indirect-test--edited
               "* H\n:LOGBOOK:\nCLOCK: [2026-11-16 Mon 10:00]--[2026-11-16 Mon 11:00] =>  1:00\n:END:\n"
               "CLOCK")
              :to-be 'org)))

  (describe "on a list"
    (it "edits the whole list from the text of an item"
      (expect (org-edit-indirect-test--edited "* H\n- one\n- two\n\nafter\n" "two")
              :to-equal "- one\n- two\n"))

    (it "edits the whole list from the start of an item"
      (expect (org-edit-indirect-test--edited "* H\n- one\n- two\n\nafter\n" "- two")
              :to-equal "- one\n- two\n"))

    (it "edits the innermost list from a nested item"
      (expect (org-edit-indirect-test--edited "- one\n  - nested\n- two\n" "nested")
              :to-equal "  - nested\n"))

    (it "edits a list inside a quote block, not the block"
      (expect (org-edit-indirect-test--edited
               "#+begin_quote\n- one\n- two\n#+end_quote\n" "one")
              :to-equal "- one\n- two\n")))

  (describe "on elements Org edits itself"
    (it "leaves tables, planning lines and keywords to Org"
      (expect (org-edit-indirect-test--edited "| a | b |\n" "a") :to-be 'org)
      (expect (org-edit-indirect-test--edited
               "* TODO H\nSCHEDULED: <2026-11-16 Mon>\n" "SCHEDULED")
              :to-be 'org)
      (expect (org-edit-indirect-test--edited "#+TITLE: foo\n" "foo") :to-be 'org))

    (it "passes the prefix argument to Org"
      (org-edit-indirect-test--with-org "#+begin_src emacs-lisp\n(+ 1 2)\n#+end_src\n"
        (org-edit-indirect-test--goto "(+ 1")
        (org-edit-indirect-special '(4))
        (expect 'org-edit-special :to-have-been-called-with '(4))))))

;;; The edit buffer

(describe "the edit buffer"
  (it "commits a body typed under an empty heading"
    (org-edit-indirect-test--with-org "* A\n** E\n* C\n"
      (org-edit-indirect-test--goto "E")
      (with-current-buffer (org-edit-indirect-special)
        (goto-char (point-max))
        (insert "body\n")
        (edit-indirect-commit))
      (expect (buffer-string) :to-equal "* A\n** E\nbody\n* C\n")))

  (it "commits a body typed under an empty heading at the end of the buffer"
    (org-edit-indirect-test--with-org "* A\n** E"
      (org-edit-indirect-test--goto "E")
      (with-current-buffer (org-edit-indirect-special)
        (goto-char (point-max))
        (insert "\nbody")
        (edit-indirect-commit))
      (expect (buffer-string) :to-equal "* A\n** E\nbody\n")))

  (it "saves, then commits, without doubling the text"
    (org-edit-indirect-test--with-org "* A\n** E\n* C\n"
      (org-edit-indirect-test--goto "E")
      (with-current-buffer (org-edit-indirect-special)
        (goto-char (point-max))
        (insert "one\n")
        (edit-indirect-save)
        (goto-char (point-max))
        (insert "two\n")
        (edit-indirect-commit))
      (expect (buffer-string) :to-equal "* A\n** E\none\ntwo\n* C\n")))

  (it "ends the committed text with a newline"
    (org-edit-indirect-test--with-org "#+begin_quote\nfoo\n#+end_quote\n"
      (org-edit-indirect-test--goto "foo")
      (with-current-buffer (org-edit-indirect-special)
        (erase-buffer)
        (insert "bar")
        (edit-indirect-commit))
      (expect (buffer-string) :to-equal "#+begin_quote\nbar\n#+end_quote\n")))

  (it "adds no second newline"
    (org-edit-indirect-test--with-org "#+begin_quote\nfoo\n#+end_quote\n"
      (org-edit-indirect-test--goto "foo")
      (with-current-buffer (org-edit-indirect-special)
        (erase-buffer)
        (insert "bar\n")
        (edit-indirect-commit))
      (expect (buffer-string) :to-equal "#+begin_quote\nbar\n#+end_quote\n")))

  (it "commits an emptied edit buffer"
    (org-edit-indirect-test--with-org "#+begin_quote\nfoo\n#+end_quote\n"
      (org-edit-indirect-test--goto "foo")
      (with-current-buffer (org-edit-indirect-special)
        (erase-buffer)
        (edit-indirect-commit))
      (expect (buffer-string) :to-equal "#+begin_quote\n#+end_quote\n")))

  (it "keeps the newline after the edit buffer changes its major mode"
    (org-edit-indirect-test--with-org "#+begin_quote\nfoo\n#+end_quote\n"
      (org-edit-indirect-test--goto "foo")
      (with-current-buffer (org-edit-indirect-special)
        (text-mode)
        (erase-buffer)
        (insert "bar")
        (edit-indirect-commit))
      (expect (buffer-string) :to-equal "#+begin_quote\nbar\n#+end_quote\n")))

  (it "adds the newline without filling the last line"
    (org-edit-indirect-test--with-org "#+begin_quote\nfoo\n#+end_quote\n"
      (org-edit-indirect-test--goto "foo")
      (with-current-buffer (org-edit-indirect-special)
        (setq-local fill-column 10)
        (auto-fill-mode 1)
        (erase-buffer)
        (insert "a line well past the fill column")
        (edit-indirect-commit))
      (expect (buffer-string)
              :to-equal
              "#+begin_quote\na line well past the fill column\n#+end_quote\n")))

  (it "opens unfolded when its major mode folds Org text"
    (let ((org-startup-folded 'overview)
          (edit-indirect-guess-mode-function (lambda (&rest _) (org-mode))))
      (org-edit-indirect-test--with-org "* H\nbody\n** S\nsub\n*** T\ndeep\n"
        (org-edit-indirect-test--goto "H")
        (with-current-buffer (org-edit-indirect-special)
          (expect (org-edit-indirect-test--invisible) :to-equal nil)))))

  (it "returns to the open edit buffer of the same element"
    (org-edit-indirect-test--with-org "* H\nbody\n"
      (org-edit-indirect-test--goto "H")
      (let* ((org (current-buffer))
             (first (org-edit-indirect-special)))
        (set-buffer org)
        (expect (org-edit-indirect-special) :to-be first))))

  (it "refuses a read-only Org buffer before it opens one"
    (org-edit-indirect-test--with-org "* H\nbody\n"
      (setq buffer-read-only t)
      (org-edit-indirect-test--goto "H")
      (expect (org-edit-indirect-special) :to-throw 'buffer-read-only)
      (expect (seq-filter #'edit-indirect-buffer-indirect-p (buffer-list))
              :to-equal nil))))

;;; The mode and the public names

(describe "org-edit-indirect-mode"
  (it "takes C-c ' over in an Org buffer"
    (org-edit-indirect-test--with-org "* H\n"
      (org-edit-indirect-mode 1)
      (expect (key-binding (kbd "C-c '")) :to-be 'org-edit-indirect-special)))

  (it "gives C-c ' back to Org when turned off"
    (org-edit-indirect-test--with-org "* H\n"
      (org-edit-indirect-mode 1)
      (org-edit-indirect-mode -1)
      (expect (key-binding (kbd "C-c '")) :to-be 'org-edit-special))))

(describe "the public names"
  (it "keep org-edit-indirect-special+ as an obsolete alias"
    (expect (indirect-function 'org-edit-indirect-special+)
            :to-be (indirect-function 'org-edit-indirect-special))
    (expect (get 'org-edit-indirect-special+ 'byte-obsolete-info)
            :not :to-be nil))

  (it "keep org-edit-indirect-generic-block out of M-x"
    (expect (commandp 'org-edit-indirect-generic-block) :to-be nil)))

;;; org-edit-indirect-tests.el ends here
