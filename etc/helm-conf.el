(use-package ivy :ensure t
  :diminish (ivy-mode . "")
  :hook (after-init . ivy-mode)
  :config
  (setq ivy-use-virtual-buffers t)
  ;;number of result lines to display
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-wrap t)
  )

(use-package counsel :ensure t
  :config
  (use-package smex :ensure t)
  :bind
  ("C-s" . swiper)
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ;;this collide
  ("C-c C-u" . counsel-unicode-char)
  ("C-c C-i" . counsel-info-lookup-symbol)
  ("C-x t" . counsel-imenu)
  ;;for git setup
  ("C-c g" . counsel-git)
  ("C-c j" . counsel-git-grep)
  ("C-c L" . counsel-git-log)
  ("C-c k" . counsel-rg)
  )

;; using consuel-tramp
(use-package counsel-tramp
  :after (counsel tramp)
  :ensure t
  :init
  (setq auth-source-save-behavior nil)  ;; don't store the password the package
  ;; does not load immediately, if you have previous opened plinkw file in
  ;; recentf, you may have error on buffer-switching, simply call counsel-tramp
  ;; to load plinkw method in
  :bind ("C-c s" . counsel-tramp)
  ;; Here is the config to make trump work on windows; forget ssh, emacs will
  ;; find /c/windows/system32/openssh first, the git ssh won't work either. For
  ;; plink to work, you have to run pink in terminal first to add it to the
  ;; REGISTRY, otherwise it will spit whole bunch of thing tramp will not
  ;; understand.
  :config
  (when (and (eq system-type 'windows-nt)  (executable-find "plink"))
    (add-to-list 'tramp-methods
		 `("plinkw"
                   (tramp-login-program "plink")
                   (tramp-login-args (("-l" "%u") ("-P" "%p") ("-t")
				      ("%h") ("\"")
				      (,(format
                                         "env 'TERM=%s' 'PROMPT_COMMAND=' 'PS1=%s'"
                                         tramp-terminal-type
                                         "$")) ;; This prompt will be
				      ("/bin/sh") ("\"")))
                   (tramp-remote-shell       "/bin/sh")
                   (tramp-remote-shell-login ("-l"))
                   (tramp-remote-shell-args  ("-c"))
                   (tramp-default-port       22)))
    )
  )
