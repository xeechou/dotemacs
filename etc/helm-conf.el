;; (use-package helm
;;   :commands (helm-imenu)
;;   :ensure helm
;;   :init		(progn
;;		  (require 'helm-config)
;;		  (require 'helm-grep)
;;		  (require 'helm-imenu)
;;					;define this minibuffer function
;;		  (defun helm-hide-minibuffer-maybe ()
;;		    (when (with-helm-buffer helm-echo-input-in-header-line)
;;		      (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
;;			(overlay-put ov 'window (selected-window))
;;			(overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
;;						`(:background ,bg-color :foreground ,bg-color)))
;;			(setq-local cursor-type nil))))
;;		  (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

;;					;I don't know why we need to unset key here
;;		  (global-unset-key (kbd "C-x c"))
;;		  (helm-mode 1)
;;		  )

;;   :bind		( ("M-x" . helm-M-x)
;;		  ("M-y" . helm-show-kill-ring)
;;		  ("C-x b" . helm-buffers-list)
;;		  ("C-x C-f" . helm-find-files)
;;		  ("C-c r" . helm-recentf)
;;		  ("C-c h o" . helm-occur)
;;		  ("C-x t" . helm-imenu)
;;		  ;;a key-map
;;		  :map helm-map
;;		  ("<tab>" . helm-execute-persistent-action)
;;		  ("C-i" . helm-execute-persistent-action)
;;		  ("C-z" . helm-select-action)
;;		  :map helm-grep-mode-map
;;		  ("<return" . helm-grep-mode-jump-other-window)
;;		  ("n" . helm-grep-mode-jump-other-window-forward)
;;		  ("p" . helm-grep-mode-jump-other-window-backward)
;;		  :map minibuffer-local-map
;;		  ("M-p" . helm-minibuffer-history)
;;		  ("M-n" . helm-minibuffer-history)

;;		 ; :map help
;;		 ; ("C-f" . helm-apropos)
;;		 ; ("r"   . helm-info-emacs)
;;		  ;("C-l" . helm-locate-library)
;;		  )

;;   :config	(setq helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
;;		      ;; helm-quick-update t
;;		      ; do not display invisible candidates
;;		      helm-ff-search-library-in-sexp t ; search for library
;;		      ;in `require' and `declare-function' sexp.

;;		      ;; you can customize helm-do-grep to execute ack-grep
;;		      ;; helm-grep-default-command "ack-grep -Hn --smart-case --no-group --no-color %e %p %f"
;;		      ;; helm-grep-default-recurse-command "ack-grep -H --smart-case --no-group --no-color %e %p %f"
;;		      helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window

;;		      helm-echo-input-in-header-line t

;;		      ;; helm-candidate-number-limit 500 ; limit the number of displayed canidates
;;		      helm-ff-file-name-history-use-recentf t
;;		      helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
;;		      helm-buffer-skip-remote-checking t

;;		      helm-mode-fuzzy-match t

;;		      helm-buffers-fuzzy-matching t ; fuzzy matching buffer names when non-nil
;;					; useful in helm-mini that lists buffers
;;		      helm-org-headings-fontify t
;;		      ;; helm-find-files-sort-directories t
;;		      ;; ido-use-virtual-buffers t
;;		      helm-semantic-fuzzy-match t
;;		      helm-M-x-fuzzy-match t
;;		      helm-imenu-fuzzy-match t
;;		      helm-lisp-fuzzy-completion t
;;		      ;; helm-apropos-fuzzy-match t
;;		      helm-buffer-skip-remote-checking t
;;		      helm-locate-fuzzy-match t
;;		      helm-display-header-line nil)
;;   )

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
