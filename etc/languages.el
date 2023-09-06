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
	 ("\\.hs\\'"     . hlsl-mode)
	 ("\\.ds\\'"     . hlsl-mode)
	 ("\\.cs\\'"     . hlsl-mode)
	 ("\\.ms\\'"     . hlsl-mode)
	 ("\\.as\\'"     . hlsl-mode)
	 ("\\.lib\\'"    . hlsl-mode)
	 ))

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

(use-package unity
  :vc (:fetcher github :repo "elizagamedev/unity.el")
  :hook (after-init . unity-mode))

;;graphviz dot
(use-package graphviz-dot-mode :ensure t
  :mode (("\\.dot\\'" . graphviz-dot-mode)))

(use-package rust-mode :ensure t :mode (("\\.rs\\'" . rust-mode)))

(use-package gdscript-mode :ensure t :mode (("\\.gd\\'" . gdscript-mode)))

(use-package markdown-mode :ensure t :mode (("\\.md\\'" . markdown-mode)))

(use-package octave :ensure t :mode (("\\.m\\'" . octave-mode)))

(use-package yaml-mode :ensure t :mode (("\\.yml\\'" . yaml-mode)))
