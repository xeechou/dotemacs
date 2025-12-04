;;; ob-lean4.el --- org-babel functions for Lean4 evaluation

;; Copyright (C) 2024

;; Author: Calude-opus-4-1
;; Keywords: literate programming, reproducible research, theorem proving
;; Homepage: https://orgmode.org
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Org-babel support for evaluating Lean4 code blocks.

;;; Requirements:

;; - Lean4 must be installed on your system
;; - lean4-mode is recommended but not required

;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-comint)
(require 'ob-eval)

;; Define file extension for Lean4
(add-to-list 'org-babel-tangle-lang-exts '("lean4" . "lean"))

;; Default header arguments for Lean4
(defvar org-babel-default-header-args:lean4 '())

;; Command to execute Lean4
(defcustom org-babel-lean4-command "lake env lean"
  "Command to run Lean4."
  :group 'org-babel
  :type 'string)

(defun org-babel-expand-body:lean4 (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (org-babel--get-vars (or processed-params
					(org-babel-process-params params)))))
    (concat
     ;; Add variable definitions if any
     (mapconcat
      (lambda (pair)
	(format "def %s := %s"
		(car pair)
		(org-babel-lean4-var-to-lean4 (cdr pair))))
      vars "\n")
     (when vars "\n\n")
     body)))

(defun org-babel-execute:lean4 (body params)
  "Execute a block of Lean4 code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (message "executing Lean4 source code block")
  (let* ((processed-params (org-babel-process-params params))
	 (result-params (cdr (assq :result-params processed-params)))
	 (result-type (cdr (assq :result-type processed-params)))
	 (full-body (org-babel-expand-body:lean4 body params processed-params))
	 (tmp-src-file (org-babel-temp-file "lean4-" ".lean"))
	 (coding-system-for-read 'utf-8)
	 (coding-system-for-write 'utf-8))

    ;; Write the Lean4 code to a temporary file
    (with-temp-file tmp-src-file
      (insert full-body))

    ;; Execute the Lean4 file
    (let ((results
	   (org-babel-eval
	    (format "%s %s"
		    org-babel-lean4-command
		    (org-babel-process-file-name tmp-src-file))
	    "")))

      ;; Process and return results based on result-type
      (org-babel-result-cond result-params
	results
	(org-babel-lean4-table-or-string results)))))

(defun org-babel-prep-session:lean4 (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  (error "Lean4 does not support sessions"))

(defun org-babel-lean4-var-to-lean4 (var)
  "Convert an elisp VAR into a string of Lean4 source code."
  (cond
   ((numberp var) (number-to-string var))
   ((stringp var) (format "\"%s\"" var))
   ((listp var)
    (format "[%s]" (mapconcat #'org-babel-lean4-var-to-lean4 var ", ")))
   (t (format "%s" var))))

(defun org-babel-lean4-table-or-string (results)
  "If RESULTS look like a table, convert them into an Emacs-lisp table.
Otherwise return the results as a string."
  (org-babel-script-escape results))

(provide 'ob-lean4)
;;; ob-lean4.el ends here
