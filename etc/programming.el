;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functional packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;magit, which I used not only for daily drive but also implementation for org
;;sync
(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package ssh-agency
  :vc (:fetcher github :repo "magit/ssh-agency")
  :hook (magit-credential . ssh-agency-ensure))

(use-package format-all
  :ensure t
  :diminish format-all-mode
  :preface
  (defun my/format-code()
    "auto formatting code in the buffer"
    (interactive)
    (if (memq 'format-all-mode local-minor-modes)
	(format-all-buffer)))
  :hook ((format-all-mode . format-all-ensure-formatter)
	 (prog-mode . format-all-mode)
	 (before-save . my/format-code))
  :custom
  (format-all-formatters (("C++" clang-format)))
  )

(use-package projectile
  :ensure t
  :diminish projectile-mode
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map))
  :custom
  (projectile-enable-caching t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; editing packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; which-key
(use-package which-key :ensure t
  :diminish which-key-mode
  :hook ((prog-mode text-mode) . which-key-mode))

(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode
  :hook ((prog-mode . whitespace-cleanup-mode)))

;; using electric pair instead of autopair
(use-package electric-pair
  :diminish electric-pair-mode
  :hook ((prog-mode text-mod outline-mode) . electric-pair-mode))

;;-3) winner-mode: undo and redo window operations, does not use very much
(use-package winner
  :defer t
  :diminish winner-mode
  ;; :bind ;;default binds, just here to remind you
  ;; (("C-c left"  .  winner-undo)
  ;;  ("C-c right" .  winner-redo))
  :hook ((prog-mode text-mode) . winner-mode))

;; visual fill column
(use-package visual-fill-column
  :ensure t
  :init
  (setq-default fill-column 79)
  :hook
  (prog-mode . turn-on-auto-fill)
  (visual-line-mode . visual-fill-column-mode)
  ((text-mode outline-mode) . visual-line-mode)
  )

;; diminish some builtin packages
(diminish 'eldoc-mode)
(diminish 'abbrev-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hideshow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hideif
  :ensure t
  :diminish hide-ifdef-mode
  :hook ((c++-mode c++-ts-mode c-mode c-ts-mode)  . hide-ifdef-mode)
  :config
  (setq hide-ifdef-read-only t)
  )

(use-package hideshow
  :hook ((prog-mode . hs-minor-mode)
	 (nxml-mode . hs-minor-mode))
  :diminish hs-minor-mode
  :bind (;; the two map didn't work, polluting global map
	 ("C-c C-h t" . hs-toggle-hiding)
	 ("C-c C-h l" . hs-hide-level)
	 ("C-c C-h a" . hs-hide-leafs)
	 ("C-c C-h s" . hs-show-block)
	 )
  :config
  (setq hs-isearch-open t)
  (add-to-list 'hs-special-modes-alist
	       '(nxml-mode
		 "<!--\\|<[^/>]*[^/]>"
		 "-->\\|</[^/>]*[^/]>"
		 "<!--"
		 sgml-skip-tag-forward
		 nil))
  :preface
  (defun hs-hide-leafs-recursive (minp maxp)
    "Hide blocks below point that do not contain further blocks in
    region (MINP MAXP)."
    (when (hs-find-block-beginning)
      (setq minp (1+ (point)))
      (funcall hs-forward-sexp-func 1)
      (setq maxp (1- (point))))
    (unless hs-allow-nesting
      (hs-discard-overlays minp maxp))
    (goto-char minp)
    (let ((leaf t))
      (while (progn
	       (forward-comment (buffer-size))
	       (and (< (point) maxp)
		    (re-search-forward hs-block-start-regexp maxp t)))
	(setq pos (match-beginning hs-block-start-mdata-select))
	(if (hs-hide-leafs-recursive minp maxp)
	    (save-excursion
	      (goto-char pos)
	      (hs-hide-block-at-point t)))
	(setq leaf nil))
      (goto-char maxp)
      leaf))
  (defun hs-hide-leafs ()
    "Hide all blocks in the buffer that do not contain subordinate
    blocks.  The hook `hs-hide-hook' is run; see `run-hooks'."
    (interactive)
    (hs-life-goes-on
     (save-excursion
       (message "Hiding blocks ...")
       (save-excursion
	 (goto-char (point-min))
	 (hs-hide-leafs-recursive (point-min) (point-max)))
       (message "Hiding blocks ... done"))
     (run-hooks 'hs-hide-hook)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functional packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company-c-headers :ensure t)
;; (setq clang-known-modes '(c++-mode c-mode))
(setq company-known-modes '(c++-mode c-mode python-mode emacs-lisp-mode cmake-mode js-mode lua-mode))
(setq enable-remote-dir-locals t)

;;--- -1) for all programming languages
;; smart-tabs mode is broken in emacs-29
;; (use-package smart-tabs-mode
;;   :ensure t
;;   :init
;;   (smart-tabs-insinuate 'c 'c++)
;;   )

(use-package paren
  :ensure t
  :diminish show-paren-mode
  :hook (prog-mode . show-paren-mode)
  :config (setq show-paren-style 'parenthesis))

(use-package fic-mode ;;show FIXME/TODO in comments
  :vc (:fetcher github :repo "lewang/fic-mode")
  :diminish fic-mode
  :hook (prog-mode . fic-mode))

(use-package color-rg
  :vc (:fetcher github :repo "manateelazycat/color-rg"))

;; yasnippet
(use-package yasnippet-snippets
  :ensure t
  :config
  (yas-reload-all)
  :hook ((prog-mode outline-mode cmake-mode) . yas-minor-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company and eglot
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :ensure t
  :defer t
  :hook (((c++-mode c++-ts-mode) . company-mode)
	 ((c-mode c-ts-mode)     . company-mode)
	 ((c++-mode c++-ts-mode c-mode c-ts-mode) .
	  (lambda () (set (make-local-variable 'company-backends)
			  '(company-capf company-files company-keywords company-dabbrev company-yasnippet))))
	 (emacs-lisp-mode . company-mode)
	 (emacs-lisp-mode . (lambda () (add-to-list (make-local-variable 'company-backends)
						    'company-elisp)))
	 (outline-mode    . company-mode) ;;enable for org mode
	 (outline-mode    . (lambda () (add-to-list (make-local-variable 'company-backends)
						    'company-dabbrev 'company-emoji)))
	 (text-mode       . company-mode)
	 (text-mode       . (lambda () (add-to-list (make-local-variable 'company-backends)
						    'company-dabbrev 'company-emoji)))
	 (meson-mode . company-mode)
	 ;;cmake
	 (cmake-mode . company-mode)
	 (cmake-mode .  (lambda () (add-to-list (make-local-variable 'company-backends)
						'company-cmake
						'company-dabbrev)))
	 ;;lua
	 (lua-mode . company-mode)
	 (lua-mode . (lambda ()
		       (add-to-list (make-local-variable 'company-backends)
				    'company-lua)))
	 )
  :config

  (setq company-minimum-prefix-length 2
	company-idle-delay 0.1
	company-async-timeout 10
	company-backends  '((company-files
			     company-keywords
			     company-yasnippet
			     company-capf)))

  (defun complete-or-indent ()
    (interactive)
    (if (company-manual-begin)
	(company-complete-common)
      (indent-according-to-mode)))
  (defun indent-or-complete ()
    (interactive)
    (if (looking-at "\\_>")
	(company-complete-common)
      (indent-according-to-mode)))
  )

(use-package company-emoji
  :defer t
  :ensure t
  :after company)

;; eglot configuration, switching to eglot after emacs 29
(use-package eglot
  :ensure t
  :hook (((c++-mode c++-ts-mode) . eglot-ensure)
	 ((c-mode c-ts-mode) . eglot-ensure)
	 (python-mode . eglot-ensure))
  :custom
  (eglot-extend-to-xref t)
  ;;inlay-hints are annoying
  (eglot-ignored-server-capabilities '(:inlayHintProvider))
  :config
  ;;by default eglot forces company to only use company-capf, I lose a lot of
  ;;backends in this way
  (setq eglot-stay-out-of '(company))
  ;;eldoc's multi-line mini buffer is really annoying, turn it off
  (setq eldoc-echo-area-use-multiline-p nil)
  ;;C++ requires clangd, python requires python-language server
  :bind (:map eglot-mode-map
	      ;; we just use the default binding here, so comment it out
	      ;; ("M-." . xref-find-definitions)
	      ;; ("M-?" . xref-find-references)
	      ;; ("M-," . xref-go-back)
	      ("C-c r"  . eglot-rename)
	      ("C-c h"  . eldoc))
  )

;; debugging with dap-mode

;; (use-package dap-mode :ensure t :defer t
;;   :commands dap-debug
;;   :after lsp-mode
;;   :config
;;   (dap-ui-mode)
;;   (dap-ui-controls-mode)
;;   (let ((dap-lldb-vscode-path (executable-find "lldb-vscode")))
;;     (when dap-lldb-vscode-path
;;       (require 'dap-lldb)
;;       (setq dap-lldb-debug-program `(, dap-lldb-vscode-path))
;;       (setq dap-lldb-debugged-program-function (lambda () (expand-file-name (read-file-name "Select file to debug."))))
;;       ))
;;   )
