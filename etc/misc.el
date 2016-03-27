;;-1)set default fill column
(setq-default fill-column 80)

;;0) save space
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/saved-places")
(setq save-place-forget-unreadable-files nil)

;;1) default text
(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)


(global-set-key (kbd  "\C-x r i") 'string-insert-rectangle)


;; 2) autopair.el
(require 'autopair)
(autopair-global-mode)

;;3) linenum
;; 3) add line numbers in, so I can jump to the line
(add-hook 'find-file-hook (lambda () (linum-mode t)))
(setq linum-format "%4d\u2502")
