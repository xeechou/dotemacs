(require 'diminish)

;; global auto revert
(global-auto-revert-mode t)

;;-4 removing the ugly tool bars
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;-3 coding system
(prefer-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)


;;0) save space
(save-place-mode 1)
(setq column-number-mode t)
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message "")
;;1) default text
(setq default-major-mode 'text-mode)
(setq make-backup-files nil)
(delete-selection-mode 1)


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
