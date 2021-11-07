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
  (when (not (getenv-internal "LANG" initial-environment))
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
