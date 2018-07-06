;; 1) this setting avoids subdirectory do not starts with letter or digit
(add-to-list 'load-path "~/.emacs.d/lisp")
(let ((default-directory "~/.emacs.d/lisp/"))
    (normal-top-level-add-subdirs-to-load-path))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; MELPA ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package) ;; you may already have this line
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize) ;;you may already have this line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; MELPA ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;install use-package if we don't have, but package refresh-content gonna take really long time
(unless (package-installed-p 'use-package)
  ;(package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(use-package diminish)
(require 'diminish)



;;now we setup themes directly in init file
(use-package monokai-theme
  :ensure t
  :init
  (load-theme 'monokai t)
  :config
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
 '(irony-additional-clang-options (quote ("-std=c++11")))
 '(package-selected-packages
   (quote
    (company-c-headers company-lsp ccls lsp-mode graphviz-dot-mode dot-mode jedi-direx jam-mode yasnippet-snippets flycheck-rtags ironyt markdown-mode company-lua company-rtags rtags lua-mode cmake-mode rjsx-mode company-tern yasnippet irony company-irony-c-headers glsl-mode paganini-theme use-package zenburn-theme rust-mode org irony-eldoc helm-gtags ggtags flycheck-rust flycheck-irony diminish company-jedi company-irony bind-key)))
 '(safe-local-variable-values
   (quote
    ((eval c-set-offset
	   (quote innamespace)
	   0)
     (eval c-set-offset
	   (quote inextern-lang)
	   0)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((t (:background "yellow" :foreground "gainsboro" :slant oblique :weight ultra-bold)))))
