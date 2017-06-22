;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Setup for programming languages ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;--- -1) for all programming languages
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(add-hook 'c++-mode-hook 'show-paren-mode)
(add-hook 'c-mode-hook 'show-paren-mode)
(setq show-paren-style 'parenthesis)




(require 'fic-mode)
(add-hook 'prog-mode-hook 'turn-on-fic-mode)
(add-hook 'c-mode-hook 'turn-on-fic-mode)
(add-hook 'c++-mode-hook 'turn-on-fic-mode)

;;--- 0) C and C++
;;setup .h to c++ mode as most people did so
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(setq c-default-style "linux"
      c-basic-offset 8)

(use-package ggtags
  :ensure t
  :init (progn
	  (add-hook 'c-mode-common-hook
		    (lambda ()
		      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
			(ggtags-mode 1))))
	  )
  :config 
  (define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
  (define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
  (define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
  (define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
  (define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
  (define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

  (define-key ggtags-mode-map (kbd "M-.")     'ggtags-find-tag-dwim)
  (define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)
  )

;; yasnippet
(use-package yasnippet
  :config
  (yas-reload-all)
  (add-hook 'c++-mode-hook #'yas-minor-mode)
  (add-hook 'c-mode-hook #'yas-minor-mode)
  )

(use-package irony
  :ensure t
  :defer t  
  :init
  (defun avoid-issue-irony-hook ()
    "load irony only if it is supported by irony."
    (when (member major-mode irony-supported-major-modes)
      (irony-mode 1))
    (when (equal major-mode 'c++-mode)
      (setq irony-additional-clang-options
	    (append '("-std=c++11") irony-additional-clang-options))))
  
  (add-hook 'c++-mode-hook 'avoid-issue-irony-hook)
  (add-hook 'c-mode-hook 'avoid-issue-irony-hook)
  
  :config
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async)
    ;; avoid enabling irony-mode in modes that inherits c-mode, e.g: php-mode
    ;(when (if (member major-mode irony-supported-major-modes) nil
					;    (irony-mode 0)))
    )
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

  (use-package irony-eldoc
    :ensure t
    :config (add-hook 'irony-mode-hook 'irony-eldoc))
  )

(use-package  company
  :ensure t
  :defer t
  :init
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'c-mode-hook  'company-mode)
  (add-hook 'python-mode-hook 'company-mode)
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  (add-hook 'cmake-mode-hook 'company-mode)
  :config
  (use-package company-irony :ensure t :defer t)
  (use-package company-jedi  :ensure t :defer t)
  (use-package company-irony-c-headers :ensure t :defer t)
  (setq company-minimum-prefix-length 2
	company-idle-delay 0.1
	company-backends  '((company-files
			     company-keywords
			     company-yasnippet))
	)
  
  (defun complete-or-indent ()
    (interactive)
    (if (company-manual-begin)
	(company-complete-common)
      (indent-according-to-mode)))
  (defun indent-or-complete ()
    (interactive)
    (if (looking-at "\\_>")
	(company-complete-common)
      (indent-according-to-mode)))

  (dolist (hook '(c-mode-hook
		  c++-mode-hook))
    (add-hook hook
	      (lambda ()
		(add-to-list (make-local-variable 'company-backends)
			     '(company-irony company-irony-c-headers)))))
  ;;;for python
  (add-hook 'python-mode-hook
	    (lambda ()
	      (add-to-list (make-local-variable 'company-backends)
			   'company-jedi)))
  ;;;for elisp
  (add-hook 'emacs-lisp-mode-hook
	    (lambda ()
	      (add-to-list (make-local-variable 'company-backends)
			   'company-elisp)))
  (add-hook 'cmake-mode-hook
	    (lambda ()
	      (add-to-list (make-local-variable 'company-backends)
			   'company-cmake)))
  
  )

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init
  (add-hook 'c-mode-hook 'flycheck-mode)
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'python-mode-hook 'flycheck-mode)
  :config
  (use-package flycheck-irony :ensure t)
  (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
  )

;; cmake
(use-package cmake-mode
  :mode (("/CMakeLists\\.txt\\'" . cmake-mode)
	 ("\\.cmake\\'" . cmake-mode)))
;; pkgbuild
(use-package pkgbuild-mode
  :ensure t
  :mode (("/PKGBUILD$" . pkgbuild-mode)))
;; glsl
(use-package glsl-mode
  :ensure t
  :mode (("\\.glsl\\'" . glsl-mode)))
(use-package cuda-mode
  :ensure t
  :mode (("\\.cu\\'" . cuda-mode)))
;;octave
(use-package octave
  :ensure t
  :mode (("\\.m\\'" . octave-mode)))
  

;; flyspell
(add-hook 'latex-mode-hook 'flyspell-mode)	     
;; 5) org-mode flyspell
(add-hook 'org-mode-hook 'flyspell-mode)

;; 6) finally, text mode should have flyspell-check
(add-hook 'text-mode-hook 'flyspell-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Setup for programming languages ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 8) hs-minor-mode
(require 'hideshow)
(require 'sgml-mode)
(require 'nxml-mode)
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)

;;Fix XML folding
(add-to-list 'hs-special-modes-alist
	     '(nxml-mode
	       "<!--\\|<[^/>]*[^/]>"
	       "-->\\|</[^/>]*[^/]>"
	       "<!--"
	       sgml-skip-tag-forward
	       nil))
(add-hook 'nxml-mode-hook 'hs-minor-mode)


;;;Hide/Show minor-mode is a much better mode to work with
(setq cm-map (make-sparse-keymap))
(global-set-key "\M-o" cm-map)
(define-key cm-map "t" 'hs-toggle-hiding)         ; Toggle hiding, which is very useful
					; HIDE
(define-key cm-map "a" 'hs-hide-all)    ; Hide everything but the top-level headings
(define-key cm-map "c" 'hs-hide-comment-region) ;Hide comment ?
(define-key cm-map "b" 'hs-hide-block)  ; Hide the block, but you may use toggle-hiding more frequently
					; SHOW
(define-key cm-map "A" 'hs-show-all)       ; Show (expand) everything
(define-key cm-map "B" 'hs-show-block)     ; Show this heading's body
