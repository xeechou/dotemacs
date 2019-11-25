(display-time)
;; 1) this setting avoids subdirectory do not starts with letter or digit
(add-to-list 'load-path "~/.emacs.d/lisp")
(let ((default-directory "~/.emacs.d/lisp/"))
    (normal-top-level-add-subdirs-to-load-path))

;;(setq warning-minimum-level :error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; MELPA ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package) ;; you may already have this line
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize) ;;you may already have this line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; MELPA ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;install use-package if we don't have, but package refresh-content gonna take really long time
(unless (package-installed-p 'use-package)
  ;(package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish)
  )

(eval-when-compile
  (require 'use-package)
  (require 'bind-key)
  (require 'diminish))

;;now we setup themes directly in init file
(use-package spacemacs-theme
  :defer t
  :ensure t
  :init
  (load-theme 'spacemacs-dark t)
  )


;;;;;;;;;;;; load user configs ;;;;;;;;;;
;; we don't need do anything specificly for flyspell-mode so long as
;; you installed hunspell, make sure your emacs version is 24+
(require 'load-dir)
(load-dir "~/.emacs.d/etc")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Custom-safe-themes
   (quote
    ("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default)))
 '(package-selected-packages
   (quote
    (whitespace-cleanup-mode flycheck smex spacemacs-theme counsel ivy lsp-ui auto-mark company-c-headers company-lsp ccls lsp-mode graphviz-dot-mode dot-mode jedi-direx jam-mode yasnippet-snippets flycheck-rtags markdown-mode company-lua company-rtags rtags lua-mode cmake-mode rjsx-mode company-tern yasnippet glsl-mode use-package rust-mode org flycheck-rust diminish company-jedi bind-key)))
 '(safe-local-variable-values
   (quote
    ((eval progn
	   (c-set-offset
	    (quote innamespace)
	    (quote +))
	   (setq c-basic-offset 4))
     (eval progn
	   (c-set-offset
	    (quote innamespace)
	    (quote +))
	   (setq c-basic-offset 4)
	   (add-hook
	    (quote c++-mode-hook)
	    (lambda nil
	      (smart-tabs-mode-enable)
	      (smart-tabs-advice c-indent-line c-basic-offset)
	      (smart-tabs-advice c-indent-region c-basic-offset))))
     (eval progn
	   (c-set-offset
	    (quote innamespace)
	    (quote +)))
     (indent-tab-mode)
     (cmake-tab-width . 4)
     (eval c-set-offset
	   (quote innamespace)
	   0)
     (eval c-set-offset
	   (quote inextern-lang)
	   0))))
 '(whitespace-style
   (quote
    (face trailing tabs spaces lines newline empty space-before-tab space-mark tab-mark newline-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((t (:background "yellow" :foreground "gainsboro" :slant oblique :weight ultra-bold)))))
