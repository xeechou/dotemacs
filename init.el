(display-time)
;; 1) this setting avoids subdirectory do not starts with letter or digit
(add-to-list 'load-path "~/.emacs.d/lisp")
(let ((default-directory "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; set for using native-compiled emacs
(when (fboundp 'native-compiled-async)
  (setq comp-deferred-compilation t
	comp-deferred-compilation-black-list '("/mu4e.*\\.el$")))
;;(setq warning-minimum-level :error)
(setq visible-bell 1)
;; disable org-roam warning
(setq org-roam-v2-ack t)

;;;;;;;;;;;;;;;;;;;;;;;;; MELPA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'package) ;; you may already have this line
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
;;(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize) ;;you may already have this line


;;install use-package if we don't have, but package refresh-content gonna take
;;really long time
(unless (package-installed-p 'use-package)
  ;; (package-refresh-contents)
  (package-install 'use-package)
  (package-install 'diminish))

;;install diminish
(unless (package-installed-p 'diminish)
  (use-package diminish :ensure t))

;; vc-use-package not yet available in 29, we need to enable it.
(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

;; available in 29 by-default
(eval-when-compile
  (require 'use-package)
  (require 'bind-key)
  (require 'diminish)
  (require 'cl-lib))

;;match the PATH from emacs to shell
(when (or (memq window-system '(mac ns x pgtk)) (daemonp))
  (use-package exec-path-from-shell
    :ensure t
    :config
    (exec-path-from-shell-initialize)))


;;;;;;;;;;;; common functions ;;;;;;;;;;
;;set org_dir
(defun my/concat-path (&rest parts)
  (cl-reduce (lambda (a b) (expand-file-name b a)) parts))

;;;;;;;;;;;; load user config ;;;;;;;;;;
;; we don't need do anything specificly for flyspell-mode so long as
;; you installed hunspell, make sure your emacs version is 24+
(require 'load-dir)
(load-dir "~/.emacs.d/etc")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(org-modern org-roam-ui simple-httpd projectile lsp-ui yasnippet-snippets
		flycheck smart-tabs-mode company-c-headers visual-fill-column
		whitespace-cleanup-mode which-key format-all magit org-contrib
		org-ref ivy-bibtex org-download org-cliplink org-roam
		mixed-pitch gdscript-mode graphviz-dot-mode lsp-dart meson-mode
		rjsx-mode shader-mode glsl-mode cmake-mode lsp-jedi python-mode
		backward-forward smex counsel-tramp flyspell-correct-popup
		flyspell-correct company-emoji exec-path-from-shell yaml-mode
		websocket vc-use-package unity typescript-mode ssh-agency
		spacemacs-theme rust-mode openwith modus-themes math-delimiters
		markdown-mode lua-mode ligature hlsl-mode go-mode fic-mode
		diminish dash dart-mode counsel color-rg))
 '(package-vc-selected-packages
   '((ligature :vc-backend Git :url "https://github.com/mickeynp/ligature.el")))
 '(safe-local-variable-values '((projectile-compilation-dir . "./build/ninja"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
