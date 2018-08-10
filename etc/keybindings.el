;;************************************************************
;;************************************************************
;;************************************************************
;;0) backward kill-line, this may not be a good idea
(defun backward-kill-line (arg)
  "Kill ARG line backwards"
  (interactive "p")
  (kill-line (- 1 arg)))
(global-set-key (kbd "C-c u") 'backward-kill-line) ;;`C-c u'
;;************************************************************
;;************************************************************
;;************************************************************





;;************************************************************
;;************************************************************
;;************************************************************
;;1) copy current lines char
(defun my-copy-line ()
  "copy current line, from the first character that is not \t or
  ' ', to the last of that line, this feature is from vim.
  Case to use this feature:
  - repeat similar lines in the code.
  "
  (interactive)
  (save-excursion
    (back-to-indentation)
    (let* ((beg (point))
	   (end (line-end-position))
	   (mystr (buffer-substring beg end)))
      (kill-ring-save beg end)
      (message "%s" mystr)))
  ;;This is silly, find a way to print out last-kill.
  )

(global-set-key "\C-c\C-k" 'my-copy-line)
;;************************************************************
;;************************************************************
;;************************************************************





;;************************************************************
;;****************************E*******************************
;;************************************************************




;;************************************************************
;;****************************S*******************************
;;************************************************************
;;3) return to previous position, this feature is very important, we need the
;; help of auto-mark, so it behaive like vim
(use-package auto-mark
  :load-path "lisp"
  :config
  (setq auto-mark-command-class-alist
	'((goto-line . jump)
	  (undo . ignore)))
  (global-auto-mark-mode 1)
  :bind ("C-c C-<SPC>"  . 'pop-to-mark-command)
  )
;; (when (require 'auto-mark nil t)

;;   (setq auto-mark-command-classifiers
;;	(list (lambda (command)
;;		(if (and (eq command 'self-insert-command)
;;			 (eq last-command-char ? ))
;;		    'ignore))))
;;   (global-auto-mark-mode 1))


;;************************************************************
;;****************************E*******************************
;;************************************************************


(defun my-next-word (p)
  "Move point to the beginning of the next word, past by any space"
  (interactive "d")
  (forward-word)
  (forward-word)
  (backward-word))
;;(global-set-key "\M-f" 'my-next-word)

;;for multiple windows convience
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
