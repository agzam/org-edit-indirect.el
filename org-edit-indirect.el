;;; org-edit-indirect.el --- Edit anything, not just source blocks -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021-2026 Ag Ibragimov
;;
;; Author: Ag Ibragimov <https://github.com/agzam>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: May, 2021
;; Version: 1.2.0
;; Keywords: convenience extensions outlines
;; Homepage: https://github.com/agzam/org-edit-indirect.el
;; Package-Requires: ((emacs "29.1") (edit-indirect "0.1.10") (org "9.6"))
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Extension for `(org-edit-special)` that includes things that are not covered, like
;; quote, verse, and comment blocks
;;
;;; Usage:
;;
;; After installing the package, add the following hook:
;;
;; (add-hook 'org-mode-hook #'org-edit-indirect-mode)
;;
;;; Code:

(require 'edit-indirect)
(require 'org)
(require 'org-element)

(defconst org-edit-indirect--blocks
  '(quote-block verse-block special-block center-block
    drawer property-drawer plain-list)
  "Element types edited whole from anywhere inside them.")

(defconst org-edit-indirect--org-objects
  '(footnote-reference inline-src-block latex-fragment timestamp)
  "Object types that `org-edit-special' has its own editor for.")

(defun org-edit-indirect--target (element)
  "Return the element to edit when point is at ELEMENT, or nil."
  (let ((type (org-element-type element)))
    (cond
     ((memq type (cons 'headline org-edit-indirect--blocks)) element)
     ((memq type '(item node-property paragraph))
      (or (org-element-lineage element org-edit-indirect--blocks)
          (and (eq type 'paragraph) element))))))

(defun org-edit-indirect--bounds (element)
  "Return (BEG . END) of ELEMENT's contents, or of ELEMENT when it has none."
  (if-let* ((beg (org-element-property :contents-begin element))
            (end (org-element-property :contents-end element)))
      (cons beg end)
    (cons (org-element-property :post-affiliated element)
          (save-excursion
            (goto-char (org-element-property :end element))
            (skip-chars-backward " \t\n")
            (line-beginning-position 2)))))

(defun org-edit-indirect--before-commit ()
  "End the edit-indirect buffer with a newline.
Otherwise the line after the edited region, such as #+end_quote, joins
its last line."
  (save-excursion
    (goto-char (point-max))
    (unless (bolp)
      (insert "\n"))))

;; Kept when the edit buffer changes its major mode.
(put 'org-edit-indirect--before-commit 'permanent-local-hook t)

(defun org-edit-indirect-generic-block (org-element)
  "Edit ORG-ELEMENT in an edit-indirect buffer and return that buffer.
The buffer holds the contents of ORG-ELEMENT, or the whole element when
it has none."
  (barf-if-buffer-read-only)
  (pcase-let ((`(,beg . ,end) (org-edit-indirect--bounds org-element)))
    (with-current-buffer (edit-indirect-region beg end :display)
      (add-hook 'edit-indirect-before-commit-hook
                #'org-edit-indirect--before-commit nil t)
      (outline-show-all)
      (current-buffer))))

(defun org-edit-indirect-special (&optional arg)
  "Edit the Org element at point in an edit-indirect buffer.
Call `org-edit-special' with ARG instead where Org has an editor of its
own, or where this package has nothing to edit."
  (interactive "P" org-mode)
  (let* ((element (org-element-at-point))
         (object (org-element-type (org-element-context element)))
         (target (unless (memq object org-edit-indirect--org-objects)
                   (org-edit-indirect--target element))))
    (if target
        (org-edit-indirect-generic-block target)
      (org-edit-special arg))))

(define-obsolete-function-alias 'org-edit-indirect-special+
  #'org-edit-indirect-special "1.2.0")

;;;###autoload
(define-minor-mode org-edit-indirect-mode
  "Edit any Org block with \\<org-mode-map>\\[org-edit-special]."
  :lighter ""
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map
              [remap org-edit-special] #'org-edit-indirect-special)
            map))

(provide 'org-edit-indirect)

;;; org-edit-indirect.el ends here
