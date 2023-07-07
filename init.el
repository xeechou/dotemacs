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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;; MELPA ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; using straight.el instead package.el for now
(let ((bootstrap-file (concat user-emacs-directory
			      "straight/repos/straight.el/bootstrap.el"))
      (bootstrap-version 3))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(eval-and-compile (require 'straight))
(setq package-enable-at-startup nil)

;; (require 'package) ;; you may already have this line
(straight-use-package 'diminish)
(straight-use-package 'use-package)
(use-package straight
  :custom (straight-use-package-by-default t))

(eval-when-compile
  (require 'use-package)
  (require 'bind-key)
  (require 'diminish)
  (require 'cl))

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
 '(Custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default))
 '(ansi-color-names-vector
   ["#000000" "#8b0000" "#00ff00" "#ffa500" "#7b68ee" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(column-number-mode t)
 '(custom-safe-themes
   '("bbb13492a15c3258f29c21d251da1e62f1abb8bbd492386a673dcfab474186af" "7fd8b914e340283c189980cd1883dbdef67080ad1a3a9cc3df864ca53bdc89cf" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "30dc9873c16a0efb187bb3f8687c16aae46b86ddc34881b7cae5273e56b97580" "dde643b0efb339c0de5645a2bc2e8b4176976d5298065b8e6ca45bc4ddf188b7" "bfc0b9c3de0382e452a878a1fb4726e1302bf9da20e69d6ec1cd1d5d82f61e3d" "36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "9b01a258b57067426cc3c8155330b0381ae0d8dd41d5345b5eddac69f40d409b" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "7aaee3a00f6eb16836f5b28bdccde9e1079654060d26ce4b8f49b56689c51904" "2d835b43e2614762893dc40cbf220482d617d3d4e2c35f7100ca697f1a388a0e" default))
 '(display-time-mode t)
 '(fci-rule-color "#383838")
 '(jdee-db-active-breakpoint-face-colors (cons "#1b1d1e" "#fc20bb"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1b1d1e" "#60aa00"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1b1d1e" "#505050"))
 '(lsp-log-io nil nil nil "Customized with use-package lsp-mode")
 '(objed-cursor-color "#d02b61")
 '(package-selected-packages
   '(auctex visual-fill-column org-ref ivy-bibtex lsp-dart dart-mode format-all magit org-bullets company-emoji flyspell-correct-popup flyspell-correct which-key lsp-jedi counsel-tramp exec-path-from-shell python-mode go-mode org-roam backward-forward typescript-mode jinja2-mode meson-mode smart-tabs-mode gdscript-mode ample-theme zenburn-theme cyberpunk-theme yaml-mode dockerfile-mode whitespace-cleanup-mode flycheck smex spacemacs-theme counsel ivy lsp-ui auto-mark company-c-headers company-lsp ccls lsp-mode graphviz-dot-mode dot-mode jedi-direx jam-mode yasnippet-snippets flycheck-rtags markdown-mode company-lua company-rtags rtags lua-mode cmake-mode rjsx-mode company-tern yasnippet glsl-mode use-package rust-mode org flycheck-rust diminish company-jedi bind-key))
 '(pdf-view-midnight-colors (cons "#dddddd" "#1b1d1e"))
 '(rustic-ansi-faces
   ["#1b1d1e" "#d02b61" "#60aa00" "#d08928" "#6c9ef8" "#b77fdb" "#00aa80" "#dddddd"])
 '(safe-local-variable-values
   '((projectile-compilation-dir . "./build/ninja")
     (projectile-compilation-dir . "./build")
     (c-file-offsets
      (innamespace . 0)
      (block-open . 0))
     (eval progn
	   (c-set-offset 'innamespace '+)
	   (setq c-basic-offset 4))
     (eval progn
	   (c-set-offset 'innamespace '+)
	   (setq c-basic-offset 4)
	   (add-hook 'c++-mode-hook
		     (lambda nil
		       (smart-tabs-mode-enable)
		       (smart-tabs-advice c-indent-line c-basic-offset)
		       (smart-tabs-advice c-indent-region c-basic-offset))))
     (eval progn
	   (c-set-offset 'innamespace '+))
     (indent-tab-mode)
     (cmake-tab-width . 4)
     (eval c-set-offset 'innamespace 0)
     (eval c-set-offset 'inextern-lang 0)))
 '(show-paren-mode t)
 '(vc-annotate-background "#1b1d1e")
 '(vc-annotate-color-map
   (list
    (cons 20 "#60aa00")
    (cons 40 "#859f0d")
    (cons 60 "#aa931a")
    (cons 80 "#d08928")
    (cons 100 "#d38732")
    (cons 120 "#d6863d")
    (cons 140 "#da8548")
    (cons 160 "#ce8379")
    (cons 180 "#c281aa")
    (cons 200 "#b77fdb")
    (cons 220 "#bf63b2")
    (cons 240 "#c74789")
    (cons 260 "#d02b61")
    (cons 280 "#b0345c")
    (cons 300 "#903d58")
    (cons 320 "#704654")
    (cons 340 "#505050")
    (cons 360 "#505050")))
 '(vc-annotate-very-old-color nil)
 '(warning-suppress-types '((ox-pandoc) (comp)))
 '(whitespace-style
   '(face trailing tabs spaces lines newline empty space-before-tab space-mark tab-mark newline-mark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((t (:background "yellow" :foreground "gainsboro" :slant oblique :weight ultra-bold)))))
