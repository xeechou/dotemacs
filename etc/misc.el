(require 'diminish)

;; global auto revert
(global-auto-revert-mode t)

;;-4 removing the ugly tool bars
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;-3 coding system
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)

;;-2) delete selection mode
(delete-selection-mode 1)
;;-1)set default fill column
(setq-default fill-column 79)
(setq make-backup-files nil)
(delete-selection-mode 1)
;;0) save space
(save-place-mode 1)
;;1) default text
(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'column-number-mode)
(add-hook 'prog-mode-hook 'column-number-mode)

;; diminish some builtin packages
(diminish 'eldoc-mode)
(diminish 'abbrev-mode)

;;-3) winner-mode
(use-package winner
  :defer t
  :diminish winner-mode
  :hook ((prog-mode text-mode) . winner-mode))

(use-package whitespace-cleanup-mode
  :ensure t
  :diminish whitespace-cleanup-mode
  :hook ((prog-mode . whitespace-cleanup-mode)))

;; 2) using electric pair instead of autopair
(use-package electric-pair
  :diminish electric-pair-mode
  :hook ((prog-mode text-mod) . electric-pair-mode))

;;3) linenum
(use-package linum
  :diminish linum-mode
  :custom (linum-format "%4d\u2502")
  :hook ((prog-mode text-mode) . linum-mode))

;;4) which-key
(use-package which-key :ensure t
  :diminish which-key-mode
  :hook ((prog-mode text-mode) . which-key-mode))


;; pdf-tools, only run this on windows
(use-package pdf-tools
  :if (eq system-type 'windows-nt)
  :defer t
  :pin manual
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-width)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  :custom
  (pdf-annot-activate-created-annotations t "automatically annotate highlights"))
