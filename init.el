;;make set-custom-variable elsewhere
(customize-save-variable 'custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (unless (file-exists-p custom-file)
;;   (write-region "(custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )" nil custom-file))


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

;; match the PATH from emacs to shell only if
;; 1. we are on linux/mac
;; 2. we start as daemon
;;
;; since windows emacs shell is cmdproxy.exe, we will manually set the
;; environment variables
(when (and (memq window-system '(mac ns x pgtk)) (daemonp))
  (use-package exec-path-from-shell
    :ensure t
    :config
    (dolist (var '("XDG_SESSION_TYPE"))
      (add-to-list 'exec-path-from-shell-variables var))
    (exec-path-from-shell-initialize)))

;;;;;;;;;;;; common functions ;;;;;;;;;;
;;set org_dir
(defun my/concat-path (&rest parts)
  (cl-reduce (lambda (a b) (expand-file-name b a)) parts))
(defun my/merge-list-to-list (dst-list src-list)
  (dolist (item src-list) (add-to-list dst-list item)))

;;;;;;;;;;;; load user config ;;;;;;;;;;
;; we don't need do anything specificly for flyspell-mode so long as
;; you installed hunspell, make sure your emacs version is 24+
(require 'load-dir)
(load-dir "~/.emacs.d/etc")

(require 'load-env-paths)
(when (eq system-type 'windows-nt) ;;use it only for windows now
  (load-env-paths "^.+_BINPATH="))

;;;;;;;;;;;; finally load user config ;;;;;;;;;;
(load custom-file)
