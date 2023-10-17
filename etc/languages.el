;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tree-sitter:  disabled for windows
;;
;; The difficult thing is to setup the indentations. See
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2023-08/msg00445.html
;; also,
;; https://casouri.github.io/note/2023/tree-sitter-starter-guide/index.html#Indentation
;; is very useful.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (treesit-available-p)
  (require 'treesit)

  (defun my/indent-rules ()
    `(;;here is my custom rule just to disable namespace indentation
      ;;(setq treesit--indent-verbose t) to see if your rule works
      ;;(treesit-check-indent c++-mode) to check your rules against c++-mode

      ((n-p-gp "declaration" "declaration_list" "namespace_definition")
       parent-bol 0)
      ((n-p-gp "comment" "declaration_list" "namespace_definition") parent-bol 0)
      ((n-p-gp "class_specifier" "declaration_list" "namespace_definition") parent-bol 0)
      ((n-p-gp "function_definition" "declaration_list" "namespace_definition")
       parent-bol 0)
      ((n-p-gp "template_declaration" "declaration_list" "namespace_definition")
       parent-bol 0)
      ,@(alist-get 'bsd (c-ts-mode--indent-styles 'cpp)))
    )

  (use-package treesit-auto
    :unless (eq system-type 'windows-nt) ;;treesit-auto does not work on windows?
    :ensure t
    :demand t
    :custom
    (c-ts-mode-indent-style #'my/indent-rules)
    :config
    (global-treesit-auto-mode)
    (setq treesit-auto-install 'prompt))
  (setq-default treesit-font-lock-level 3)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Elisp
(use-package paredit
  :ensure t :defer t :pin melpa
  :hook ( (emacs-lisp-mode lisp-interaction-mode) . paredit-mode))

(use-package rainbow-delimiters
  :ensure t :defer t
  :hook ((emacs-lisp-mode lisp-interaction-mode) . rainbow-delimiters-mode))

;; C family
(use-package cc-mode
  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
	 ("\\.m\\'" . c-mode)
	 ("\\.mm\\'" . c++-mode)
	 ("\\.inl\\'" . c++-mode))
  :preface
  (defun my/cmode-hook ()
    ;;default settings
    (setq c-default-style "linux"
	  c-basic-offset 8)
    (c-set-offset 'inextern-lang 0)
    (c-set-offset 'innamespace 0)
    (c-set-offset 'inline-open 0)
    )
  :hook
  ((c-mode-common . my/cmode-hook)))


;;cmake
(use-package cmake-mode
  :ensure t
  :config
  :mode (("/CMakeLists\\.txt\\'" . cmake-mode)
	 ("\\.cmake\\'" . cmake-mode)))

;; glsl
(use-package glsl-mode
  :ensure t
  :mode (("\\.glsl\\'" . glsl-mode)
	 ("\\.vert\\'" . glsl-mode)
	 ("\\.frag\\'" . glsl-mode)
	 ("\\.geom\\'" . glsl-mode)
	 ("\\.comp\\'" . glsl-mode)
	 ("\\.rgen\\'" . glsl-mode)
	 ("\\.rchit\\'" . glsl-mode)
	 ("\\.rmiss\\'" . glsl-mode))
  )

;; hlsl
(use-package hlsl-mode
  :vc (:fetcher github :repo "xeechou/hlsl-mode.el")
  :mode (("\\.fxh\\'"    . hlsl-mode)
	 ("\\.hlsl\\'"   . hlsl-mode)
	 ("\\.vs\\'"     . hlsl-mode)
	 ("\\.ps\\'"     . hlsl-mode)
	 ("\\.hs\\'"     . hlsl-mode) ;;hull shader
	 ("\\.ds\\'"     . hlsl-mode) ;;domain shader
	 ("\\.cs\\'"     . hlsl-mode) ;;compute shader
	 ("\\.ms\\'"     . hlsl-mode) ;;mesh shader
	 ("\\.as\\'"     . hlsl-mode) ;;amplification shader
	 ("\\.lib\\'"    . hlsl-mode) ;;ray-tracing shader library
	 ))

(use-package azsl-mode
  :vc (:fetcher github :repo "xeechou/azsl-mode.el")
  :mode (("\\.azsl\\'"  . azsl-mode)
	 ("\\.azsli\\'" . azsl-mode)))

(use-package shader-mode
  :ensure t
  :mode (("\\.shader\\'" . hlsl-mode)))

;; golang
(use-package go-mode
  :ensure t
  :mode (("\\.go\\'" . go-mode)
	 ("\\.mode\\'" . go-mode))
  :hook ((go-mode . (lambda () (add-hook 'before-save-hook 'gofmt-before-save nil t)))))

;;javascript
(use-package rjsx-mode
  :ensure t
  :defer t
  :mode (("\\.js\\'" . rjsx-mode))
  :config (setq js-indent-level 2)
  )

(use-package web-mode
  :ensure t
  :pin melpa
  :defer t
  :mode ("\\.html?\\'" . web-mode))

;;typescript
(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2)
  (setq-default indent-tabs-mode nil)
  )

(use-package json-mode
  :ensure t
  :pin melpa
  :mode (("\\.json\\'" . json-mode)
	 ;; O3DE passes and assets use json format
	 ("\\.pass\\'"    . json-mode)
	 ("\\.azasset\\'" . json-mode)
	 ))

;;mesonbuild
(use-package meson-mode
  :ensure t
  :defer t
  :mode (("/meson\\.build\\'" . meson-mode))
  )

;;lua
(use-package lua-mode
  :ensure t
  :mode (("\\.lua\\'" . lua-mode)))

;;dart
(use-package dart-mode
  :ensure t
  :defer t
  :mode (("\\.dart\\'" . dart-mode))
  :config
  (with-eval-after-load 'projectile
    (projectile-register-project-type 'flutter '("pubspec.yaml")
				      :project-file "pubspec.yaml"
				      :compile "flutter build"
				      :test "flutter test"
				      :run "flutter run"
				      :src-dir "lib/"))
  )



;; (use-package tex :ensure auctex
;;   :custom
;;   (TeX-master              nil)
;;   (Tex-auto-save           t)
;;   (Tex-parse-self          t)
;;   (Tex-save-query          nil)
;;   (reftex-plug-into-AUCTeX t)
;;   :hook
;;   ((latex-mode . flyspell-mode)
;;    (latex-mode . turn-on-reftex)
;;    (LaTeX-mode . turn-on-reftex))
;;   )

;; (use-package unity
;;   :vc (:fetcher github :repo "elizagamedev/unity.Eli")
;;   :hook (after-init . unity-mode))

;;graphviz dot
(use-package graphviz-dot-mode :ensure t
  :mode (("\\.dot\\'" . graphviz-dot-mode)))

(use-package rust-mode :ensure t :mode (("\\.rs\\'" . rust-mode)))

(use-package gdscript-mode :ensure t :mode (("\\.gd\\'" . gdscript-mode)))

(use-package markdown-mode :ensure t :mode (("\\.md\\'" . markdown-mode)))

(use-package octave :ensure t :mode (("\\.m\\'" . octave-mode)))

(use-package yaml-mode :ensure t :mode (("\\.yml\\'" . yaml-mode)))
