;;; pgit.el --- A lightweight project manaagement utility using git

;; Copyright (c) 2014 Andreas Zapf

;; Author: Andreas Zapf
;; Version: 0.1
;; Keywords: projects git

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A lightweight project manaagement utility using git.

;;; Code:

(require 'cl-lib)

(defvar pgit-root nil
  "The path of the root of a buffer's working copy, if the buffer
belongs to a project. Automatically set during loading a file
that is part of a pgit-project")

(defun pgit--add-to-safe-local-variables (variable-value-list)
  (dolist (variable-value-pair variable-value-list)
    (add-to-list 'safe-local-variable-values variable-value-pair)))

(defun pgit--for-each-list-of-variables (fun dir-local-class-variables)
  (let ((variables-without-modes (mapcar 'cdr dir-local-class-variables)))
    (mapc fun variables-without-modes)))

(defun pgit-define-project-class
  (symbol &optional pgit-variables other-variables)
  "Sets SYMBOL to define a pgit project class which can then be
associated with a directory to make it a project
(`pgit-make-project').

PGIT-VARIABLES is an alist with elements of the form

  (VARIABLE . VALUE)

OTHER-VARIABLES is a list of the form

  (MAJOR-MODE . ALIST)

where MAJOR-MODE is a symbol for a major mode and ALIST has
members like PGIT-VARIABLES.

This function is built on top of
`dir-locals-set-class-variables', which it passes the given lists
of variables. It extends that function by adding the given values
to `safe-local-variable-values' so that users won't have to
answer confirmation questions every time they open a file in a
project."

  (let ((pgit-vars (copy-tree pgit-variables)))
    (add-to-list 'pgit-vars '(eval . (pgit-set-pgit-root)))
    (let ((all-vars
           `((nil . ,pgit-vars) . ,(copy-tree other-variables))))
      (pgit--for-each-list-of-variables
       'pgit--add-to-safe-local-variables
       all-vars)
      (dir-locals-set-class-variables symbol all-vars))))

(defalias 'pgit-make-project 'dir-locals-set-directory-class
  "Call to make the given directory a project of the given class.")

(defun pgit-set-pgit-root ()
  "Sets the variable pgit-root buffer-locally to the root of the
current git working copy. Should be called when setting
directory-local variables belonging to a project.

FIXME: maybe it'd be sufficient to set this via a file-load hook,
if there was such a thing"
  (require 'vc-hooks)
  (set (make-local-variable 'pgit-root) (vc-find-root default-directory ".git")))

;;; pgit.el ends here
