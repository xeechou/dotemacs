(use-package flyspell
  :if (or (executable-find "ispell") (executable-find "hunspell") (executable-find "aspell"))
  :defer t
  :hook ((prog-mode . flyspell-prog-mode)
	 (text-mode . flyspell-mode) ;;for markdown, org, nxml
	 ;;also disable it for specific mode
	 (change-log-mode . (turn-off-flyspell)))
  :init ;;for flyspell to work, you need to set LANG first
  (when (not (getenv "LANG"))
    (setenv "LANG" "en_US"))
  ;;:config
  ;;TODO flyspell language-tool
  )
;; correcting word and save it to personal dictionary
(use-package flyspell-correct
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map ("C-c ;" . flyspell-correct-wrapper))
  )
(use-package flyspell-correct-popup
  :ensure t
  :after (ivy flyspell-correct))


;; flyspell
;; (setq flyspell-issue-message-flag nil)
;; (defun spell-switch-dictionary()
;;   (interactive)
;;   (let* ((dic ispell-current-dictionary)
;;         (change (if (string= dic "francais") "english" "francais")))
;;     (ispell-change-dictionary change)
;;     (message "Dictionary switched from %s to %s" dic change)
;;     ))
;; (global-set-key (kbd "<f8>")   'spell-switch-dictionary)
;; (add-hook 'latex-mode-hook 'flyspell-mode)
;; (add-hook 'org-mode-hook 'flyspell-mode)
;; (add-hook 'text-mode-hook 'flyspell-mode)
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
