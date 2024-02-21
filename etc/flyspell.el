(use-package flyspell
  :if (or (executable-find "ispell") (executable-find "hunspell") (executable-find "aspell"))
  :defer t
  :hook ((prog-mode . flyspell-prog-mode)
	 (text-mode . flyspell-mode) ;;for markdown, org, nxml
	 ;;also disable it for specific mode
	 (change-log-mode . (turn-off-flyspell)))
  :init
  ;;for flyspell to work, you need to set LANG first
  ;; on windows, getenv has strange behavior, getenv-internal seems to work correctly.
  ;; (when (not (getenv-internal "LANG" initial-environment))
  (setenv "LANG" "en_US")
  :custom  (ispell-program-name (or (executable-find "hunspell")
				    (executable-find "aspell")
				    (executable-find "ispell")))
  ;;:config
  ;;TODO flyspell language-tool
  )
;; correcting word and save it to personal dictionary
(use-package flyspell-correct
  :ensure t
  :after flyspell
  :bind (:map flyspell-mode-map ("C-c ;" . flyspell-correct-wrapper))
  )

(use-package flyspell-correct-ivy
  ;;switch to use ivy interface
  ;;TODO there is a face bug on popup interface
  ;;NOTE: use M-o to access ivy menus
  :ensure t
  :after (ivy flyspell-correct))
