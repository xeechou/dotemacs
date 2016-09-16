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
;;****************************S*******************************
;;************************************************************
;;2) taglist
(require 'taglist)
(global-set-key "\C-xt" 'taglist)
;;************************************************************
;;****************************E*******************************
;;************************************************************





;;************************************************************
;;****************************S*******************************
;;************************************************************
;;3) return to previous position, this feature is very important, we need the
;; help of auto-mark, so it behaive like vim
(when (require 'auto-mark nil t)
  (setq auto-mark-command-class-alist
	'((anything . anything)
	  (goto-line . jump)
	  (indent-for-tab-command . ignore)
	  (undo . ignore)))
  (setq auto-mark-command-classifiers
	(list (lambda (command)
		(if (and (eq command 'self-insert-command)
			 (eq last-command-char ? ))
		    'ignore))))
  (global-auto-mark-mode 1))

(global-set-key (kbd "C-c M-p") 'pop-to-mark-command)
;;************************************************************
;;****************************E*******************************
;;************************************************************





;;;Hide/Show minor-mode is a much better mode to work with
(setq cm-map (make-sparse-keymap))
(global-set-key "\M-o" cm-map)
(define-key cm-map "t" 'hs-toggle-hiding)         ; Toggle hiding, which is very useful
; HIDE
(define-key cm-map "a" 'hs-hide-all)    ; Hide everything but the top-level headings
(define-key cm-map "c" 'hs-hide-comment-region) ;Hide comment ?
(define-key cm-map "b" 'hs-hide-block)  ; Hide the block, but you may use toggle-hiding more frequently
; SHOW
(define-key cm-map "A" 'hs-show-all)       ; Show (expand) everything
(define-key cm-map "B" 'hs-show-block)     ; Show this heading's body

