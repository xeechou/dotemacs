;;-2) delete selection mode
(delete-selection-mode 1)
;;-1)set default fill column
(setq-default fill-column 80)
(setq make-backup-files nil)
(delete-selection-mode 1)
;;0) save space
(save-place-mode 1)
;;1) default text
(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)


(global-set-key (kbd  "\C-x r i") 'string-insert-rectangle)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; 2) autopair.el
(require 'autopair)
(autopair-global-mode)

;;3) linenum
;; 3) add line numbers in, so I can jump to the line
(add-hook 'find-file-hook (lambda () (linum-mode t)))
(setq linum-format "%4d\u2502")
