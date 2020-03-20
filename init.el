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
(use-package ample-theme
  :defer t
  :ensure t
  :init
  (load-theme 'ample t)
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
 '(ansi-color-names-vector
   ["#000000" "#8b0000" "#00ff00" "#ffa500" "#7b68ee" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(custom-safe-themes
   (quote
    ("36ca8f60565af20ef4f30783aa16a26d96c02df7b4e54e9900a5138fb33808da" "9b01a258b57067426cc3c8155330b0381ae0d8dd41d5345b5eddac69f40d409b" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "bf798e9e8ff00d4bf2512597f36e5a135ce48e477ce88a0764cfb5d8104e8163" "c9ddf33b383e74dac7690255dd2c3dfa1961a8e8a1d20e401c6572febef61045" "7aaee3a00f6eb16836f5b28bdccde9e1079654060d26ce4b8f49b56689c51904" "2d835b43e2614762893dc40cbf220482d617d3d4e2c35f7100ca697f1a388a0e" default)))
 '(fci-rule-color "#383838")
 '(jdee-db-active-breakpoint-face-colors (cons "#1b1d1e" "#fc20bb"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1b1d1e" "#60aa00"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1b1d1e" "#505050"))
 '(lsp-log-io nil)
 '(lsp-prefer-flymake nil t)
 '(lsp-print-io nil)
 '(lsp-print-performance nil)
 '(lsp-trace nil t)
 '(lsp-ui-flycheck-enable t t)
 '(lsp-ui-sideline-enable t)
 '(lsp-ui-sideline-ignore-duplicate t)
 '(lsp-ui-sideline-show-diagnostics t)
 '(objed-cursor-color "#d02b61")
 '(package-selected-packages
   (quote
    (ample-theme zenburn-theme cyberpunk-theme yaml-mode dockerfile-mode doom-theme whitespace-cleanup-mode flycheck smex spacemacs-theme counsel ivy lsp-ui auto-mark company-c-headers company-lsp ccls lsp-mode graphviz-dot-mode dot-mode jedi-direx jam-mode yasnippet-snippets flycheck-rtags markdown-mode company-lua company-rtags rtags lua-mode cmake-mode rjsx-mode company-tern yasnippet glsl-mode use-package rust-mode org flycheck-rust diminish company-jedi bind-key)))
 '(pdf-view-midnight-colors (cons "#dddddd" "#1b1d1e"))
 '(rustic-ansi-faces
   ["#1b1d1e" "#d02b61" "#60aa00" "#d08928" "#6c9ef8" "#b77fdb" "#00aa80" "#dddddd"])
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
 '(whitespace-style
   (quote
    (face trailing tabs spaces lines newline empty space-before-tab space-mark tab-mark newline-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((t (:background "yellow" :foreground "gainsboro" :slant oblique :weight ultra-bold)))))
