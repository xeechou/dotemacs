;; ;; setup theme and fonts for Emacs
;; change theme based on time of day
;;currently tried theme
;; 1. spaceemacs-theme
;; 2. modus-themes ;;
;; 3. apropospriate-theme :: low contrast, seems pretty good

(use-package modus-themes
  ;; TODO have to disable defer to get circadian to work
  :ensure t
  :init
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-common-palette-overrides
	`(
	  ;; From the section "Make the mode line borderless"
	  (border-mode-line-active unspecified)
	  (border-mode-line-inactive unspecified))))

(use-package circadian
  :ensure t
  :config
  (setq circadian-themes
	'(("8:00" . modus-operandi-tinted)
	  ("17:30" . modus-vivendi-tinted)))
  (circadian-setup))

;; Ligature Settings
(use-package ligature
  :vc (:fetcher github :repo "mickeynp/ligature.el")
  :if (string-match "HARFBUZZ" system-configuration-features)
  :hook ((prog-mode text-mode) . ligature-mode)
  :config
  ;; Enable "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www")))

(use-package appr
  :load-path "lisp"
  :hook (after-init . appr)
  :custom
  (appr-default-font-size 13)
  (appr-cjk-font-list     '("WenQuanYi Micro Hei"
			    "WenQuanYi Zen Hei"
			    "Microsoft YaHei"
			    "Microsoft JhengHei"))
  (appr-emoji-font-list '("Noto Color Emoji"
			  "Noto Emoji"
			  "Segoe UI Emoji"
			  "Symbola"
			  "Apple Color Emoji"))

  (appr-variable-pitch-font-list '("Fira Sans"
				   "Iosevka Aile")))
