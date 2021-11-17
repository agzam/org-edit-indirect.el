;;; org-edit-indirect.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Ag Ibragimomv
;;
;; Author: Ag Ibragimomv <https://github.com/agzam>
;; Maintainer: Ag Ibragimomv <agzam.ibragimov@gmail.com>
;; Created: May, 2021
;; Version: 1.0.0
;; Keywords: convenience extensions outlines
;; Homepage: https://github.com/agzam/org-edit-indirect.el
;; Package-Requires: ((emacs "25.1") (edit-indirect "0.1.6") (org "9.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;; Edit Org-mode elements like `org-edit-special' but for things that not covered, like
;; quote, verse and comment blocks
;;
;;; Code:

(require 'edit-indirect)
(require 'org-element)

(defun org-edit-indirect-generic-block (org-element)
  (interactive)
  (let* ((el-type (org-element-type (org-element-context org-element)))
         (parent (org-element-property :parent org-element))
         (beg (pcase el-type
                ((or `quote-block `verse-block `comment-block `plain-list)
                 (org-element-property :contents-begin org-element))
                (_ (org-element-property :begin (or parent org-element)))))
         (end (pcase el-type
                ((or `quote-block `verse-block `comment-block `plain-list)
                 (org-element-property :contents-end org-element))
                (_ (org-element-property :end (or parent org-element))))))
    (edit-indirect-region beg end :display)))

(defun org-edit-indirect-special+ (&optional arg)
  "Call a special editor fore the element at point.
This fn extends `org-edit-special', allowing to edit blocks that
the original function doesn't let you."
  (interactive "P")
  (let* ((element (org-element-at-point))
         (context (org-element-context element)))
    (pcase (org-element-type context)
      ((or `quote-block `verse-block `comment-block
           `paragraph `headline `property-drawer
           `plain-list `item)
          (org-edit-indirect-generic-block element))
      (_ (org-edit-special arg)))))

(defun org-edit-indirect--before-commit ()
  ;; if not done this way, edit-indirect chews up the EOF and #+end_quote ends
  ;; up appended to the previous line, breaking the structure of the block
  (when (edit-indirect-buffer-indirect-p)
   (goto-char (point-max))
   (forward-char -1)
   (when (not (looking-at "$"))
     (goto-char (point-max))
     (newline))))

(add-hook 'edit-indirect-before-commit-hook #'org-edit-indirect--before-commit)
(add-hook 'edit-indirect-after-creation-hook #'outline-show-all)

(define-key org-mode-map (kbd "C-c '") #'org-edit-indirect-special+)

(provide 'org-edit-indirect)

;;; org-edit-indirect.el ends here
