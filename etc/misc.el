;;-3) winner-mode
(use-package winner :defer t :diminish :init (winner-mode 1))

;;-2) delete selection mode
(delete-selection-mode 1)
;;-1)set default fill column
(setq-default fill-column 79)
(setq make-backup-files nil)
(delete-selection-mode 1)
;;0) save space
(save-place-mode 1)
;;1) default text
(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'column-number-mode)
(add-hook 'prog-mode-hook 'column-number-mode)

(use-package whitespace-cleanup-mode
  :ensure t
  :hook ((prog-mode . whitespace-cleanup-mode)))

;; 2) autopair.el
(use-package autopair :load-path "lisp/"
  :config (autopair-global-mode))

;;3) linenum
;; 3) add line numbers in, so I can jump to the line
(add-hook 'find-file-hook (lambda () (linum-mode t)))
(setq linum-format "%4d\u2502")

;; flyspell
(setq flyspell-issue-message-flag nil)
(defun spell-switch-dictionary()
  (interactive)
  (let* ((dic ispell-current-dictionary)
	 (change (if (string= dic "francais") "english" "francais")))
    (ispell-change-dictionary change)
    (message "Dictionary switched from %s to %s" dic change)
    ))
(global-set-key (kbd "<f8>")   'spell-switch-dictionary)
(add-hook 'latex-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'flyspell-mode)
;;  ((executable-find "hunspell")
;;   (setq ispell-program-name "hunspell")
;;   (setq ispell-local-dictionary "en_US")
;;   ;; ispell-set-spellchecker-params has to be called
;;   ;; before ispell-hunspell-add-multi-dic will work
;;   ;;(ispell-set-spellchecker-params)
;; ;;  (ispell-hunspell-add-multi-dic "en_US,fr_CA")
;;   )
;;  ((executable-find "aspell")
;;   (setq ispell-program-name "aspell"))
;;  )

;; 5) org-mode flyspell

;; 6) finally, text mode should have flyspell-check
