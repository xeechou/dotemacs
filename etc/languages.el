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


(use-package irony
  :ensure t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'cmake-mode-hook 'company-mode)
  (add-hook 'python-mode-hook 'company-mode)
  
  :config
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  
  (defun my-cpp-hook ()
    (setq irony-additional-clang-options
	  (append '("-std=c++11") irony-additional-clang-options)))
  (add-hook 'c++-mode-hook 'my-cpp-hook)
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

  (use-package irony-eldoc
    :ensure t
    :config (add-hook 'irony-mode-hook 'irony-eldoc))
  )


(use-package  company
  :ensure t
  :init
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'c-mode-hook  'company-mode)
  (add-hook 'emacs-lisp-mode-hook 'company-mode)  

  :config
  (use-package company-irony :ensure t :defer t)
  (setq company-minimum-prefix-length 2
	company-idle-delay 0.1
	company-backends  '((company-irony company-gtags))
	)
  
  )

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init
  (add-hook 'prog-mode-hook #'flycheck-mode)
  :config
  (use-package flycheck-irony :ensure t)
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
  )



;; --3) for cmake
;; --4) for octave-mode
					;(autoload 'octave-mode "octave" nil t)
					;(setq auto-mode-alist
					;      (cons '("\\*.m$" . octave-mode) auto-mode-alist))
;; flyspell
(add-hook 'latex-mode-hook 'flyspell-mode)	     
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


;; 9) python
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))
(add-hook 'python-mode-hook 'my/python-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Setup for programming languages ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 8) hs-minor-mode
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
