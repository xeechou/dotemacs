;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; functional packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company-c-headers :ensure t)
(setq clang-known-modes '(c++-mode c-mode))
(setq company-known-modes '(c++-mode c-mode python-mode emacs-lisp-mode cmake-mode js-mode lua-mode))

;;--- -1) for all programming languages
(use-package smart-tabs-mode
  :ensure t
  :init
  (smart-tabs-insinuate 'c 'c++)
)

(use-package flycheck
  :ensure t
  :commands flycheck-mode
  :hook ((c++-mode c-mode) . flycheck-mode))

(use-package paren
  :ensure t
  :diminish show-paren-mode
  :hook (prog-mode . show-paren-mode)
  :config (setq show-paren-style 'parenthesis))

(use-package fic-mode
  :load-path "lisp/"
  :diminish fic-mode
  :config
  (add-hook 'prog-mode-hook 'turn-on-fic-mode)
  )

(use-package color-rg :load-path "lisp/" )

;; yasnippet
(use-package yasnippet-snippets
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'cmake-mode-hook #'yas-minor-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lsp setup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lsp-mode
  :ensure t
  :hook ((c++-mode . lsp)
	 (c-mode . lsp)
	 (python-mode . lsp))
  :commands (lsp lsp-deferred)
  :init (setq lsp-auto-guess-root t)
  :custom
  (lsp-print-io nil)
  (lsp-trace nil)
  (lsp-print-performance nil)
  (lsp-prefer-flymake nil)
  :config
  (use-package lsp-ui
    :ensure t
    :commands lsp-ui-mode
    :init
    (add-hook 'lsp-mode-hook 'lsp-ui-mode)
    :bind (:map lsp-ui-mode-map
		;; remember M-, (which is xref function) to jump back
		("M-." . lsp-ui-peek-find-definitions)
		("M-?" . lsp-ui-peek-find-references)
		("C-x t" . lsp-ui-imenu)))
  :custom
  (lsp-ui-sideline-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-diagnostics t)
  (lsp-ui-flycheck-enable t)
  (lsp-ui-doc-position 'bottom)
    ;; ;;don't create lsp-stderr buffer
    ;; ;;I need to read lsp-ui code
    ;; (setq lsp-ui-flycheck-enable t
    ;;	  lsp-ui-imenu-enable t
    ;;	  lsp-ui-sideline-ignore-duplicate t)
  )

(use-package company
  :ensure t
  :defer t
  :hook ((c-mode . company-mode)
	 (c++-mode . company-mode)
	 (emacs-lisp-mode . company-mode)
	 (cmake-mode . company-mode))
  :config
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; general setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq company-minimum-prefix-length 2
	company-idle-delay 0.1
	company-async-timeout 10
	company-backends  '((company-files
			     company-keywords
			     company-yasnippet))
	)

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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; lanuages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Backend-setup;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (dolist (hook '(c-mode-hook
		  c++-mode-hook))
    (add-hook hook
	      (lambda ()
		(add-to-list (make-local-variable 'company-backends)
			     'company-capf))))
  ;;;for python
  (add-hook 'python-mode-hook
	    (lambda ()
	      (add-to-list (make-local-variable 'company-backends)
			   'company-jedi)))
  ;;;for elisp
  (add-hook 'emacs-lisp-mode-hook
	    (lambda ()
	      (add-to-list (make-local-variable 'company-backends)
			   'company-elisp)))

  (add-hook 'cmake-mode-hook
	    (lambda ()
	      (add-to-list (make-local-variable 'company-backends)
			   'company-cmake)))
  (add-hook 'lua-mode-hook
	    (lambda ()
	      (add-to-list (make-local-variable 'company-backends)
			   'company-lua)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C family
(use-package cc-mode
  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
	 ("\\.m\\'" . c-mode)
	 ("\\.mm\\'" . c++-mode)
	 ("\\.inl\\'" . c++-mode))
  :preface
  (defun my-cmode-hook ()
    (setq c-default-style "linux"
	  c-basic-offset 8)
    (c-set-offset 'inextern-lang 0)
    (c-set-offset 'innamespace 0)
    (c-set-offset 'inline-open 0)
    )
  :hook
  (c-mode-common . my-cmode-hook)
  )

;;cmake
(use-package cmake-mode
  :ensure t
  :mode (("/CMakeLists\\.txt\\'" . cmake-mode)
	 ("\\.cmake\\'" . cmake-mode)))
;; glsl
(use-package glsl-mode
  :ensure t
  :mode (("\\.glsl\\'" . glsl-mode)
	 ("\\.vs\\'" . glsl-mode)
	 ("\\.vert\\'" . glsl-mode)
	 ("\\.fs\\'" . glsl-mode)
	 ("\\.frag\\'" . glsl-mode)
	 ("\\.gs\\'" . glsl-mode)
	 ("\\.comp\\'" . glsl-mode))
  )
;;javascript
(use-package rjsx-mode
  :ensure t
  :defer t
  :mode (("\\.js\\'" . rjsx-mode))
  :config (setq js-indent-level 2)
  )
;;typescript
(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2)
  (setq-default indent-tabs-mode nil)
  )
;;mesonbuild
(use-package meson-mode
  :ensure t
  :defer t
  :hook (meson-mode . company-mode)
  :mode (("/meson\\.build\\'" . meson-mode))
  )
;;graphviz dot
(use-package graphviz-dot-mode :ensure t
  :mode (("\\.dot\\'" . graphviz-dot-mode)))

(use-package lua-mode :ensure t :mode (("\\.lua\\'" . lua-mode)))

(use-package rust-mode :ensure t :mode (("\\.rs\\'" . rust-mode)))

(use-package gdscript-mode :ensure t :mode (("\\.gd\\'" . gdscript-mode)))

(use-package markdown-mode :ensure t :mode (("\\.md\\'" . markdown-mode)))

(use-package octave :ensure t :mode (("\\.m\\'" . octave-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hideshow
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package hideif
  :ensure t
  :diminish hide-ifdef-mode
  :hook (c-mode-common . hide-ifdef-mode)
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
	 ("C-c C-h s" . hs-show-all)
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
  ;;---- define the leaf function
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
