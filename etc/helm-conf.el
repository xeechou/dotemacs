(use-package ivy :ensure t
  :diminish (ivy-mode . "")
  :bind
  (:map ivy-mode-map  ("C-'" . ivy-avy))
  :config
  (ivy-mode 1)
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
  ("C-c k" . counsel-ag)
  )
