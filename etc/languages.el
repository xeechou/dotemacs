;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Setup for programming languages ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;--- -1) for all programming languages
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(add-hook 'c++-mode-hook 'show-paren-mode)
(add-hook 'c-mode-hook 'show-paren-mode)
(setq show-paren-style 'parenthesis)

(add-hook 'c-mode-common-hook (lambda()
				(c-set-offset 'inextern-lang 0)
				(c-set-offset 'innamespace 0)))
;;default C indent level
(setq c-default-style "linux"
      c-basic-offset 8)
;;before use-package
(add-to-list 'auto-mode-alist '(".h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))


(use-package fic-mode
  :load-path "lisp/"
  :config
  (add-hook 'prog-mode-hook 'turn-on-fic-mode)
  (add-hook 'c-mode-hook 'turn-on-fic-mode)
  (add-hook 'c++-mode-hook 'turn-on-fic-mode)
  )

;; yasnippet
(use-package yasnippet-snippets
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'cmake-mode-hook #'yas-minor-mode)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;company;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package  company
  :ensure t
  :defer t
  :init
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'c-mode-hook  'company-mode)

  (add-hook 'python-mode-hook 'company-mode)
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  (add-hook 'cmake-mode-hook 'company-mode)
  (add-hook 'js-mode-hook  'company-mode)
  (add-hook 'lua-mode-hook 'company-mode)
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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;setting up flycheck;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package flycheck
    :ensure t
    :diminish flycheck-mode
    :init
    (add-hook 'c-mode-hook 'flycheck-mode)
    (add-hook 'c++-mode-hook 'flycheck-mode)
    (add-hook 'python-mode-hook 'flycheck-mode)
    :config
    (use-package flycheck-irony
      :ensure t
      :config
      (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
      (when (equal major-mode 'c++-mode)
	(setq flycheck-clang-language-standard "c++14"))
      (when (equal major-mode 'c-mode)
	(setq flycheck-clang-language-standard "c11"))
      )
    )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; C++ setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package ccls
    :ensure t
    :defer t
    :commands (lsp-ccls-enable)
    :init
    (defun my-ccls-enable ()
      (condition-case nil
	  (lsp-ccls-enable)
      (user-error nil))
      )
    (add-hook 'c-mode-hook 'my-ccls-enable)
    (add-hook 'c++-mode-hook 'my-ccls-enable)
    :config
    (setq ccls-executable (string-trim-right (shell-command-to-string "which ccls")))
    (setq ccls-extra-init-params
	  '(:clang (:extraArgs ("-D__cpp_deduction_guides=0" "-Wno-macro-redefined"))
		   ))
    ;;there should be other settings. I need to get it work first
    (with-eval-after-load 'projectile
      (add-to-list 'projectile-globally-ignored-directories ".ccls-cache"))

    (use-package company-c-headers :ensure t :defer t)
    (use-package company-lsp :ensure t :defer t)
    (require 'lsp-imenu)
    (add-hook 'lsp-after-open-hook 'lsp-enable-imenu)
    )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; python setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package company-jedi
    :ensure t
    :defer t
    :config
    (use-package jedi-direx :ensure t :defer t
      :config (add-hook 'jedi-mode-hook 'jedi-direx:setup))
    :bind (:map python-mode-map
		("M-," . jedi:goto-definition)
		("M-." . jedi:goto-definition-next)
		)
    )
  ;;other packages
  (use-package company-lua   :ensure t :defer t)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Backend-setup;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (dolist (hook '(c-mode-hook
		  c++-mode-hook))
    (add-hook hook
	      (lambda ()
		(add-to-list (make-local-variable 'company-backends)
			     '(company-lsp company-c-headers)))))
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




;;languages
;;cmake
(use-package cmake-mode
  :ensure t
  :mode (("/CMakeLists\\.txt\\'" . cmake-mode)
	 ("\\.cmake\\'" . cmake-mode)))
;; pkgbuild
(use-package pkgbuild-mode
  :ensure t
  :mode (("/PKGBUILD\\'" . pkgbuild-mode)))
;; glsl
(use-package glsl-mode
  :ensure t
  :mode (("\\.glsl\\'" . glsl-mode)
	 ("\\.vs\\'" . glsl-mode)
	 ("\\.fs\\'" . glsl-mode)
	 ("\\.gs\\'" . glsl-mode)))
(use-package cuda-mode
  :ensure t
  :mode (("\\.cu\\'" . cuda-mode)))
;;octave
(use-package octave
  :ensure t
  :mode (("\\.m\\'" . octave-mode)))
;;special javascript
(setq js-indent-level 2)
(use-package rjsx-mode
  :ensure t
  :mode (("\\.js\\'" . rjsx-mode)))

(use-package lua-mode
  :ensure t
  :mode (("\\.lua\\'" . lua-mode)))
(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)))
(use-package jam-mode
  :ensure t
  :mode (("\\.v2\\'" . jam-mode)
	 ("/Jamfile\\." . jam-mode)
	 ("\\.jam\\'" . jam-mode)))

(use-package rust-mode
  :ensure t
  :mode (("\\.rs\\'" . rust-mode)))

(use-package graphviz-dot-mode
  :ensure t
  :mode (("\\.dot\\'" . graphviz-dot-mode)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Setup for programming languages ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 8) hs-minor-mode
(require 'hideshow)
(require 'sgml-mode)
(require 'nxml-mode)
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'python-mode-hook     'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(add-hook 'rust-mode-hook       'hs-minor-mode)

;;Fix XML folding
(add-to-list 'hs-special-modes-alist
	     '(nxml-mode
	       "<!--\\|<[^/>]*[^/]>"
	       "-->\\|</[^/>]*[^/]>"
	       "<!--"
	       sgml-skip-tag-forward
	       nil))
(add-hook 'nxml-mode-hook 'hs-minor-mode)

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

;;;Hide/Show minor-mode is a much better mode to work with
(setq cm-map (make-sparse-keymap))
(global-set-key "\M-o" cm-map)
(define-key cm-map "t" 'hs-toggle-hiding)         ; Toggle hiding, which is very useful
					; HIDE
(define-key cm-map "a" 'hs-hide-leafs)    ; Hide everything but the top-level headings
(define-key cm-map "c" 'hs-hide-comment-region) ;Hide comment ?
(define-key cm-map "l" 'hs-hide-level)
(define-key cm-map "A" 'hs-show-all)       ; Show (expand) everything
(define-key cm-map "B" 'hs-show-block)     ; Show this heading's body
