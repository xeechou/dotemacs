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

;;-3) winner-mode
(use-package winner
  :defer t
  :diminish winner-mode
  :hook ((prog-mode text-mode) . winner-mode))

(use-package whitespace-cleanup-mode
  :ensure t
  :hook ((prog-mode . whitespace-cleanup-mode)))

;; 2) using electric pair instead of autopair
(use-package electric-pair
  :diminish electric-pair-mode
  :hook ((prog-mode text-mod) . electric-pair-mode))

;;3) linenum
(use-package linum
  :diminish linum-mode
  :custom (linum-format "%4d\u2502")
  :hook ((prog-mode text-mode) . linum-mode))

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

;; pdf-tools, only run this on windows
(use-package pdf-tools
  :if (eq system-type 'windows-nt)
  :defer t
  :pin manual
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))
