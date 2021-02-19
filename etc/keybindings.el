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

;;1) move lines
(defmacro save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))
(put 'save-column 'lisp-indent-function 0)

(defun move-line-up ()
  (interactive)
  (save-column
   (transpose-lines 1)
   (forward-line -2)))

(defun move-line-down ()
  (interactive)
  (save-column
   (forward-line 1)
   (transpose-lines 1)
   (forward-line -1)))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

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
;;****************************S*******************************
;;************************************************************

;;3) this backward-forward package helps us jump back-forward in the mark ring.
(use-package backward-forward
  :ensure t
  :demand
  :config
  (backward-forward-mode t)
  :bind (:map backward-forward-mode-map
              ("<C-left>" . nil)
              ("<C-right>" . nil)
              ("<M-left>" . backward-forward-previous-location)
              ("<M-right>" . backward-forward-next-location)
              ("<mouse-8>" . backward-forward-previous-location)
              ("<mouse-9>" . backward-forward-next-location)
              )
  )
;; (use-package auto-mark
;;   :load-path "lisp"
;;   :config
;;   (setq auto-mark-command-class-alist
;;	'((goto-line . jump)
;;	  (undo . ignore)))
;;   (global-auto-mark-mode 1)
;;   :bind ("C-c C-<SPC>"  . 'pop-to-mark-command)
;;   )

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

;;************************************************************
;;************************************************************
;;************************************************************
(global-set-key (kbd  "\C-x r i") 'string-insert-rectangle)
