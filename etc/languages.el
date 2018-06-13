;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Setup for programming languages ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;--- -1) for all programming languages
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(add-hook 'c++-mode-hook 'show-paren-mode)
(add-hook 'c-mode-hook 'show-paren-mode)
(setq show-paren-style 'parenthesis)

(add-hook 'c-mode-common-hook (lambda()
				(c-set-offset 'inextern-lang 0)
				(c-set-offset 'innamespace 0)))
;;default C indent level
(setq c-default-style "linux"
      c-basic-offset 8)
;;before use-package
(add-to-list 'auto-mode-alist '(".h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.inl\\'" . c++-mode))


(use-package fic-mode
  :load-path "lisp/"
  :config
  (add-hook 'prog-mode-hook 'turn-on-fic-mode)
  (add-hook 'c-mode-hook 'turn-on-fic-mode)
  (add-hook 'c++-mode-hook 'turn-on-fic-mode)
  )

;; yasnippet
(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook #'yas-minor-mode)
  (add-hook 'cmake-mode-hook #'yas-minor-mode)
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;company;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package  company
  :ensure t
  :defer t
  :init
  (add-hook 'c++-mode-hook 'company-mode)
  (add-hook 'c-mode-hook  'company-mode)

  (add-hook 'python-mode-hook 'company-mode)
  (add-hook 'emacs-lisp-mode-hook 'company-mode)
  (add-hook 'cmake-mode-hook 'company-mode)
  (add-hook 'js-mode-hook  'company-mode)
  (add-hook 'lua-mode-hook 'company-mode)
  :config
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; general setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq company-minimum-prefix-length 2
	company-idle-delay 0.1
	company-async-timeout 10
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

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;setting up flycheck;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package flycheck
    :ensure t
    :diminish flycheck-mode
    :init
    (add-hook 'c-mode-hook 'flycheck-mode)
    (add-hook 'c++-mode-hook 'flycheck-mode)
    (add-hook 'python-mode-hook 'flycheck-mode)
    :config
    (use-package flycheck-irony
      :ensure t
      :config
      (add-hook 'flycheck-mode-hook #'flycheck-irony-setup)
      (when (equal major-mode 'c++-mode)
	(setq flycheck-clang-language-standard "c++14"))
      (when (equal major-mode 'c-mode)
	(setq flycheck-clang-language-standard "c11"))
      )
    )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; C++ setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package irony
    :ensure t
    :defer t
    :init
    ;;if I add this line: (delete 'c++-mode-hook 'company-senmatic-backend)
    ;;shit will go wrong
    :config
    ;;enable the windows system bindings
    (if (string-equal system-type "windows-nt")
	(progn
	    (setq w32-pipe-read-delay 0)
	    (setq irony-server-w32-pipe-buffer-size (* 64 1024)))
      )
    (defun avoid-issue-irony-hook ()
      "load irony only if it is supported by irony."
      (when (member major-mode irony-supported-major-modes)
	(irony-mode 1))
      (when (equal major-mode 'c++-mode)
	(setq irony-additional-clang-options
	      (delete-dups
	       (append '("-std=c++14") irony-additional-clang-options))))
      )

    (add-hook 'c++-mode-hook 'avoid-issue-irony-hook)
    (add-hook 'c-mode-hook 'avoid-issue-irony-hook)

    (defun my-irony-mode-hook ()
      (define-key irony-mode-map [remap completion-at-point]
	'irony-completion-at-point-async)
      (define-key irony-mode-map [remap complete-symbol]
	'irony-completion-at-point-async)
      )

    (add-hook 'irony-mode-hook 'my-irony-mode-hook)
    (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

    (use-package irony-eldoc
      :ensure t
      :config (add-hook 'irony-mode-hook 'irony-eldoc))
    (use-package company-irony :ensure t :defer t)
    (use-package company-irony-c-headers :ensure t :defer t)
    ;;;;;setup the Rtags as well in the irony
    (if (not (string-equal system-type "windows-nt"))
	(use-package rtags
	  :ensure t
	  :defer  t
	  :config
	  (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
	  (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
	  (unless (file-exists-p "/usr/bin/rdm")
	    (setq rtags-path (concat (getenv "HOME") "/.bin/"))
	    )
	  ;; (defun my-close-rtags-taglist ()
	  ;;   "close rtags-tagslist when in the taglist"
	  ;;   (interactive)
	  ;;   (windmove-right)
	  ;;   (rtags-close-taglist)
	  ;;   (kill-buffer "*RTags*")
	  ;;   )
	  ;;define the keybindings
	  ;; Je n'aime pas le rtags-taglist, il a trop des etiquette que J'ai besoin.
	  ;; (define-key rtags-taglist-mode-map (kbd "q") 'my-close-rtags-taglist)
	  ;; (define-key c-mode-base-map (kbd "C-x t") 'rtags-taglist)
	  (define-key c-mode-base-map (kbd "M-,") 'rtags-find-references-at-point)
	  (define-key c-mode-base-map (kbd "M-.") 'rtags-find-symbol-at-point)
	  )
      )
    )
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; python setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (use-package company-jedi
    :ensure t
    :defer t
    :config
    (use-package jedi-direx :ensure t :defer t
      :config (add-hook 'jedi-mode-hook 'jedi-direx:setup))
    :bind (:map python-mode-map
		("M-," . jedi:goto-definition)
		("M-." . jedi:goto-definition-next)
	   )
    )
  ;;other packages
  (use-package company-lua   :ensure t :defer t)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Backend-setup;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  (add-hook 'lua-mode-hook
	    (lambda ()
	      (add-to-list (make-local-variable 'company-backends)
			   'company-lua)))
  )




;;languages
;;cmake
(use-package cmake-mode
  :ensure t
  :mode (("/CMakeLists\\.txt\\'" . cmake-mode)
	 ("\\.cmake\\'" . cmake-mode)))
;; pkgbuild
(use-package pkgbuild-mode
  :ensure t
  :mode (("/PKGBUILD\\'" . pkgbuild-mode)))
;; glsl
(use-package glsl-mode
  :ensure t
  :mode (("\\.glsl\\'" . glsl-mode)
	 ("\\.vs\\'" . glsl-mode)
	 ("\\.fs\\'" . glsl-mode)
	 ("\\.gs\\'" . glsl-mode)))
(use-package cuda-mode
  :ensure t
  :mode (("\\.cu\\'" . cuda-mode)))
;;octave
(use-package octave
  :ensure t
  :mode (("\\.m\\'" . octave-mode)))
;;special javascript
(setq js-indent-level 2)
(use-package rjsx-mode
  :ensure t
  :mode (("\\.js\\'" . rjsx-mode)))

(use-package lua-mode
  :ensure t
  :mode (("\\.lua\\'" . lua-mode)))
(use-package markdown-mode
  :ensure t
  :mode (("\\.md\\'" . markdown-mode)))
(use-package jam-mode
  :ensure t
  :mode (("\\.v2\\'" . jam-mode)
	 ("/Jamfile\\." . jam-mode)
	 ("\\.jam\\'" . jam-mode)))

(use-package rust-mode
  :ensure t
  :mode (("\\.rs\\'" . rust-mode)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Setup for programming languages ;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 8) hs-minor-mode
(require 'hideshow)
(require 'sgml-mode)
(require 'nxml-mode)
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'python-mode-hook     'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)
(add-hook 'rust-mode-hook       'hs-minor-mode)

;;Fix XML folding
(add-to-list 'hs-special-modes-alist
	     '(nxml-mode
	       "<!--\\|<[^/>]*[^/]>"
	       "-->\\|</[^/>]*[^/]>"
	       "<!--"
	       sgml-skip-tag-forward
	       nil))
(add-hook 'nxml-mode-hook 'hs-minor-mode)

(defun hs-hide-leafs-recursive (minp maxp)
  "Hide blocks below point that do not contain further blocks in
    region (MINP MAXP)."
  (when (hs-find-block-beginning)
    (setq minp (1+ (point)))
    (funcall hs-forward-sexp-func 1)
    (setq maxp (1- (point))))
  (unless hs-allow-nesting
    (hs-discard-overlays minp maxp))
  (goto-char minp)
  (let ((leaf t))
    (while (progn
	     (forward-comment (buffer-size))
	     (and (< (point) maxp)
		  (re-search-forward hs-block-start-regexp maxp t)))
      (setq pos (match-beginning hs-block-start-mdata-select))
      (if (hs-hide-leafs-recursive minp maxp)
	  (save-excursion
	    (goto-char pos)
	    (hs-hide-block-at-point t)))
      (setq leaf nil))
    (goto-char maxp)
    leaf))

(defun hs-hide-leafs ()
  "Hide all blocks in the buffer that do not contain subordinate
    blocks.  The hook `hs-hide-hook' is run; see `run-hooks'."
  (interactive)
  (hs-life-goes-on
   (save-excursion
     (message "Hiding blocks ...")
     (save-excursion
       (goto-char (point-min))
       (hs-hide-leafs-recursive (point-min) (point-max)))
     (message "Hiding blocks ... done"))
   (run-hooks 'hs-hide-hook)))

;;;Hide/Show minor-mode is a much better mode to work with
(setq cm-map (make-sparse-keymap))
(global-set-key "\M-o" cm-map)
(define-key cm-map "t" 'hs-toggle-hiding)         ; Toggle hiding, which is very useful
					; HIDE
(define-key cm-map "a" 'hs-hide-leafs)    ; Hide everything but the top-level headings
(define-key cm-map "c" 'hs-hide-comment-region) ;Hide comment ?
(define-key cm-map "l" 'hs-hide-level)
(define-key cm-map "A" 'hs-show-all)       ; Show (expand) everything
(define-key cm-map "B" 'hs-show-block)     ; Show this heading's body
