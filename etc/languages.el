;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Setup for programming languages ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;--- -1) for all programming languages
(require 'fic-mode)
(add-hook 'prog-mode-hook 'turn-on-fic-mode)
(add-hook 'c-mode-hook 'turn-non-fic-mode)
(add-hook 'c++-mode-hook 'turn-non-fic-mode)

;;--- 0) C and C++
;;setup .h to c++ mode as most people did so
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(setq c-default-style "linux"
      c-basic-offset 8)
;; auto-complete using 'irony-mode
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'company-mode)
(add-hook 'c-mode-hook  'company-mode)

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;; the packages in the official repo has problem, we have to install them manually
(require 'irony-eldoc)
(add-hook 'irony-mode-hook 'irony-eldoc)
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))


(eval-after-load 'company
     '(add-to-list 'company-backends 'company-irony))


(setq company-minimum-prefix-length 2
      company-idle-delay 0.1)



;;---1) for elisp
(add-hook 'emacs-lisp-mode-hook 'company-mode)
;;---2) latex, flyspell
(add-hook 'latex-mode-hook 'flyspell-mode)	     


;; --3) for cmake

(require 'cmake-mode)
(add-hook 'cmake-mode-hook 'company-mode)

;; --4) for octave-mode
;(autoload 'octave-mode "octave" nil t)
;(setq auto-mode-alist
;      (cons '("\\*.m$" . octave-mode) auto-mode-alist))

;; 5) org-mode flyspell
(add-hook 'org-mode-hook 'flyspell-mode)

;; 6) finally, text mode should have flyspell-check
(add-hook 'text-mode-hook 'flyspell-mode)

;; 7) glsl-mode
(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))

;; 8) pkgbuild-mode
(autoload 'pkgbuild-mode "pkgbuild-mode.el" "PKGBUILD mode." t)
(add-to-list 'auto-mode-alist '("/PKGBUILD$" .pkgbuild-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Setup for programming languages ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
