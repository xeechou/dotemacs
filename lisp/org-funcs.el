;;; org-funcs.el my common routines for org mode


;;; Commentary:

;; org-funcs defines functions I used for org mode which are too big to place
;; inside config file directly

;; TODO
;; 1. writing small hooks to get md5/sha1 on save an org-file;
;; 2. Using json or simply a file-list to store the file name and md5/sha1
;; 3. When transmitting. comparing the md5/sha1, only copy files that has
;;    changed.

(require 'org)

;;;###autoload
(defun org-funcs-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are don, to TODO otherwise"
  (let (org-log-done org-log-states) ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

;; At the courtesy of https://emacs.stackexchange.com/questions/19843/how-to-automatically-adjust-an-org-task-state-with-its-children-checkboxes

;;;###autoload
(defun org-funcs-checkbox-todo ()
  "Switch header TODO state to DONE when all checkboxes are ticked, to TODO otherwise"
  (let ((todo-state (org-get-todo-state)) beg end)
    (unless (not todo-state)
      (save-excursion
	(org-back-to-heading t)
	(setq beg (point))
	(end-of-line)
	(setq end (point))
	(goto-char beg)
	(if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
			       end t)
            (if (match-end 1)
		(if (equal (match-string 1) "100%")
		    (unless (string-equal todo-state "DONE")
		      (org-todo 'done))
		  (unless (string-equal todo-state "TODO")
		    (org-todo "TODO")))
	      (if (and (> (match-end 2) (match-beginning 2))
		       (equal (match-string 2) (match-string 3)))
		  (unless (string-equal todo-state "DONE")
		    (org-todo "DONE"))
		(unless (string-equal todo-state "TODO")
		  (org-todo "TODO")))))))))




(provide 'org-funcs)
