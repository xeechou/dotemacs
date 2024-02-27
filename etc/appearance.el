;;load light dark theme based on time
(defun my/theme-based-on-time (light-theme dark-theme)
  "get the right theme based on the time of day"
  (let* ((time  (current-time-string))
	 (clock (nth 3 (split-string time)))
	 (h-m-s (split-string clock ":"))
	 (hour  (string-to-number (car h-m-s))))
    (if (and (>= hour 8) (<= hour 17)) light-theme dark-theme)))

;; setup theme and fonts for Emacs
(use-package modus-themes
  :defer t
  :ensure t
  :init
  (setq modus-themes-mixed-fonts t)
  (setq modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-tinted))
  (setq modus-themes-common-palette-overrides
	`(
	  ;; From the section "Make the mode line borderless"
	  (border-mode-line-active unspecified)
	  (border-mode-line-inactive unspecified)))
  ;; #' is for quoting function, like ' is for quoting symbol
  (run-with-timer 0 3600
		  #'(lambda () (modus-themes-select (my/theme-based-on-time
						     'modus-operandi-tinted
						     'modus-vivendi-tinted)))))

;; Ligature Settings
(use-package ligature
  :vc (:fetcher github :repo "mickeynp/ligature.el")
  :if (string-match "HARFBUZZ" system-configuration-features)
  :hook ((prog-mode text-mode) . ligature-mode)
  ;; Enable all Cascadia Code ligatures in programming modes
  :init
  (setq ligature-monolisa-symbols '("-->" "->" "->>" "-<" "--<"
				    "-~" "]#" ".-" "!=" "!=="
				    "#(" "#{" "#[" "#_" "#_("
				    "/=" "/==" "|||" "||" ;; "|"
				    "==" "===" "==>" "=>" "=>>"
				    "=<<" "=/" ">-" ">->" ">="
				    ">=>" "<-" "<--" "<->" "<-<"
				    "<!--" "<|" "<||" "<|||"
				    "<|>" "<=" "<==" "<==>" "<=>"
				    "<=<" "<<-" "<<=" "<~" "<~>"
				    "<~~" "~-" "~@" "~=" "~>"
				    "~~" "~~>" ".=" "..=" "---"
				    "{|" "[|" ".."  "..."  "..<"
				    ".?"  "::" ":::" "::=" ":="
				    ":>" ":<" ";;" "!!"  "!!."
				    "!!!"  "?."  "?:" "??"  "?="
				    "**" "***" "*>" "*/" "#:"
				    "#!"  "#?"  "##" "###" "####"
				    "#=" "/*" "/>" "//" "///"
				    "&&" "|}" "|]" "$>" "++"
				    "+++" "+>" "=:=" "=!=" ">:"
				    ">>" ">>>" "<:" "<*" "<*>"
				    "<$" "<$>" "<+" "<+>" "<>"
				    "<<" "<<<" "</" "</>" "^="
				    "%%" "'''" "\"\"\"" ))

  (setq ligature-cascadia-symbols '("|||>" "<|||" "<==>" "<!--" "####" "~~>"
				    "***" "||=" "||>" ":::" "::=" "=:=" "==="
				    "==>" "=!=" "=>>" "=<<" "=/=" "!==" "!!."
				    ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->"
				    "---" "-<<" "<~~" "<~>" "<*>" "<||" "<|>"
				    "<$>" "<==" "<=>" "<=<" "<->" "<--" "<-<"
				    "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_("
				    "..<" "..." "+++" "/==" "///" "_|_" "www"
				    "&&" "^=" "~~" "~@" "~=" "~>" "~-" "**"
				    "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-"
				    "{|" "[|" "]#" "::" ":=" ":>" ":<" "$>"
				    "==" "=>" "!=" "!!" ">:" ">=" ">>" ">-"
				    "-~" "-|" "->" "--" "-<" "<~" "<*" "<|"
				    "<:" "<$" "<=" "<>" "<-" "<<" "<+" "</"
				    "#{" "#[" "#:" "#=" "#!" "##" "#(" "#?"
				    "#_" "%%" ".=" ".-" ".." ".?" "+>" "++"
				    "?:" "?=" "?." "??" ";;" "/*" "/=" "/>"
				    "//" "__" "~~" "(*" "*)" "\\\\" "://"))

  (setq ligature-fira-symbols '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
				"{-" "::" ":::" ":=" "!!" "!=" "!==" "-}" "----"
				"-->" "->" "->>" "-<" "-<<" "-~" "#{" "#[" "##"
				"###" "####" "#(" "#?" "#_" "#_(" ".-" ".=" ".."
				"..<" "..." "?=" "??" ";;" "/*" "/**" "/=" "/=="
				"/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^="
				"$>" "++" "+++" "+>" "=:=" "==" "===" "==>" "=>"
				"=>>" "<=" "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-"
				">>=" ">>>" "<*" "<*>" "<|" "<|>" "<$" "<$>"
				"<!--" "<-" "<--" "<->" "<+" "<+>" "<=" "<=="
				"<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
				"<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))

  (setq ligature-losevka-symbols '("<---" "<--"  "<<-" "<-" "->" "-->" "--->"
				   "<->" "<-->" "<--->" "<---->" "<!--"
				   "<==" "<===" "<=" "=>" "=>>" "==>" "===>"
				   ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
				   "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!="
				   "===" "!==" ":=" ":-" ":+" "<*" "<*>" "*>"
				   "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++"
				   "+++"))

  (setq ligature-jetbrain-symbols '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>"
				    "-->" "///" "/=" "/==" "/>" "//" "/*" "*>"
				    "***" "*/" "<-" "<<-" "<=>" "<=" "<|" "<||"
				    "<|||" "<|>" "<:" "<>" "<-<" "<<<" "<=="
				    "<<=" "<=<" "<==>" "<-|" "<<" "<~>" "<=|"
				    "<~~" "<~" "<$>" "<$" "<+>" "<+" "</>" "</"
				    "<*" "<*>" "<->" "<!--" ":>" ":<" ":::" "::"
				    ":?" ":?>" ":=" "::=" "=>>" "==>" "=/="
				    "=!=" "=>" "===" "=:=" "==" "!==" "!!" "!="
				    ">]" ">:" ">>-" ">>=" ">=>" ">>>" ">-" ">="
				    "&&&" "&&" "|||>" "||>" "|>" "|]" "|}" "|=>"
				    "|->" "|=" "||-" "|-" "||=" "||" ".." ".?"
				    ".=" ".-" "..<" "..." "+++" "+>" "++" "[||]"
				    "[<" "[|" "{|" "??" "?." "?=" "?:" "##" "###"
				    "####" "#[" "#{" "#=" "#!" "#:" "#_(" "#_"
				    "#?" "#(" ";;" "_|_" "__" "~~" "~~>" "~>"
				    "~-" "~@" "$>" "^=" "]#"))
  :config
  ;; Enable "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www")))

(defun my/ligature-set-for-symbol (symbols)
  (ligature-set-ligatures 'prog-mode symbols)
  (ligature-set-ligatures 'text-mode symbols))


;; Setting fonts. We are taking chances on different systems,
;;
;; This one is bit tricky, when you start Emacs as daemon, the font-family-list
;; will be nil for non

(defun my/font-setup ()
  ;; Set main font
  (cond
   ((member "Fira Code" (font-family-list)) ;; ligature fonts
    (set-frame-font "Fira Code-13" t t)
    (when (boundp 'ligature-fira-symbols)
      (my/ligature-set-for-symbol ligature-fira-symbols)))
   ;; Cascadia fonts is available for windows-terminal
   ((member "Cascadia Code" (font-family-list))
    (set-frame-font "Cascadia Code-13" t t)
    (when (boundp 'ligature-cascadia-symbols)
      (my/ligature-set-for-symbol ligature-cascadia-symbols)))
   ;; JetBrains mono is also a good ligature font
   ((member "JetBrains Mono" (font-family-list))
    (set-frame-font "JetBrains Mono-13" t t)
    (when (boundp 'ligature-jetbrains-symbols)
      (my/ligature-set-for-symbol ligature-jetbrains-symbols)))
   ;; Windows: Consolas should be available
   ((member "Consolas" (font-family-list))
    (set-frame-font "Consolas-13" t t))
   ;; Darwin:
   ((member "SF Mono" (font-family-list)) ;;2017+
    (set-frame-font "SF Mono-13" t t))
   ((member "Menlo" (font-family-list))
    (set-frame-font "Menlo-13" t t))
   ;;Linux fonts
   ((member "DejaVu Sans Mono" (font-family-list))
    (set-frame-font "DejaVu Sans Mono-13" t t)))

  ;; Set font for Chinese
  (cond
   ;; MS Windows
   ((member "Microsoft YaHei" (font-family-list))
    (set-fontset-font t 'han  "Microsoft YaHei"))
   ((member "Microsoft JhengHei" (font-family-list))
    (set-fontset-font t 'han  "Microsoft JhengHei"))
   ((member "SimHei" (font-family-list))
    (set-fontset-font t 'han  "SimHei"))
   ;; Apple
   ((member "Hei" (font-family-list))
    (set-fontset-font t 'han  "Hei"))
   ((member "Hei SC" (font-family-list))
    (set-fontset-font t 'han  "Hei SC"))
   ((member "Hei TC" (font-family-list))
    (set-fontset-font t 'han  "Hei TC"))
   ;; Linux && BSD
   ((member "WenQuanYi Micro Hei" (font-family-list))
    (set-fontset-font t 'han  "WenQuanYi Micro Hei")))

  ;; set font for emoji
  (cond
   ((member "Noto Color Emoji" (font-family-list))
    (set-fontset-font t '(#x1f300 . #x1fad0) "Noto Color Emoji"))
   ((member "Noto Emoji" (font-family-list))
    (set-fontset-font t '(#x1f300 . #x1fad0) "Noto Emoji"))
   ((member "Segoe UI Emoji" (font-family-list))
    (set-fontset-font t '(#x1f300 . #x1fad0) "Segoe UI Emoji"))
   ((member "Symbola" (font-family-list))
    (set-fontset-font t '(#x1f300 . #x1fad0) "Symbola"))
   ((member "Apple Color Emoji" (font-family-list))
    (set-fontset-font t '(#x1f300 . #x1fad0) "Apple Color Emoji"))
   ;; Apple Color Emoji should be before Symbola, but Richard Stallman disabled it.
   ;; GNU Emacs Removes Color Emoji Support on the Mac
   ;; http://ergoemacs.org/misc/emacs_macos_emoji.html
   )
  )

(if (daemonp)
    ;;only available for Emacs clients
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		"Configure faces on frame connection"
		(when frame
		  (select-frame frame))
		(message "Setup up font for current frame")
		(when (display-graphic-p)
		  (my/font-setup))))
  ;; starting Emacs individually
  (add-hook 'window-setup-hook #'my/font-setup))
