(use-package org-mode
  :ensure org
  :mode (("\\.org$" . org-mode))
  :custom
  (org-directory "~/org/")
  :init
  (setq org-todo-keywords
	'((sequence "TODO" "DOIN" "|" "PEND" "DONE" "CANC")))
  (setq org-todo-keyword-faces
	'(("TODO" . error)
	  ("DOIN" . org-document-title)
	  ("DONE" . org-level-5)
	  ("CANC" . org-level-4)
	  ("PEND" . org-level-3)))
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  ;; org-latex-preview settings, requires we have program latex and dvipng
  (setq org-latex-create-formula-image-program 'dvipng)
  (setq org-preview-latex-image-directory
	(concat temporary-file-directory "ltximg/"))

  (setq org-default-notes-file (concat org-directory "miscs.org"))
  ;; you have to set this before loading org-mode
  (setq org-agenda-files (list (concat org-directory "work.org")
			       (concat org-directory "training.org")
			       (concat org-directory "goals-habits.org")
			       (concat org-directory "miscs.org")
			       (concat org-directory "social.org")))
  ;; it seems if we use org-mobile-files, it is the only list we move
  (setq org-mobile-files (append org-agenda-files
				 (list (concat org-directory "notes.org")
				       (concat org-directory "journal.org")
				       (concat org-directory "today.org"))))
  (setq org-log-done 'time)
  ;; recursively update the parents TODO
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are don, to TODO otherwise"
    (let (org-log-done org-log-states) ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  :hook (org-after-todo-statistics . org-summary-todo)
  ;I am not sure this global key setting is good or not, capture stuff globally
  ;is great
  :bind (:map global-map
	      ("\C-ca" . org-agenda)
	      ("\C-cc" . org-capture))
  :config
  ;;activate babel languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp  . t)
     (shell       . t)
     (python      . (if (executable-find "python") t nil))
     (C           . (if (and (executable-find "gcc")
			     (executable-find "g++"))
			t nil))
     ))
  )

;; org-roam minor mode
(use-package org-roam
  :ensure t
  :init (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (concat org-directory "roam/"))
  :bind  (("C-c n r" . org-roam-buffer-toggle) ;;toggle-back-links
	  ("C-c n f" . org-roam-node-find)
	  ("C-c n c" . org-roam-capture)
	  ("C-c n i" . org-roam-node-insert)
	  ("C-c n g" . org-roam-graph)) ;; doesn't work
  :config
  ;;start db sync automatically, also you are able to refresh backlink buffer
  (org-roam-db-autosync-enable)
  (setq org-roam-completion-system 'ivy)
  ;;setup for windows
  (when (eq system-type 'windows-nt)
    (setq org-roam-db-update-method 'immediate))
  ;; template for v2
  (setq org-roam-capture-templates
        '(
          ("d" "default" plain "%?"
	   :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                     "#+title: ${title}\n#+filetags: %^{org-roam-tags}\n#+created: %u\n")
           :unnarrowed t
           :jump-to-captured t)

          ("l" "clipboard" plain (function org-roam--capture-get-point)
           "%c"
           :file-name "%<%Y-%m-%d-%H-%M-%S>-${slug}"
           :head "#+title: ${title}\n#+created: %u\n#+last_modified: %U\n\
#+ROAM_TAGS: %?\n"
           :unnarrowed t
           :prepend t
           :jump-to-captured t)
          ))
  ;; configure org-roam-buffer
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
		 (display-buffer-in-direction)
		 (direction . right)
		 (window-width . 0.33)
		 (window-height . fit-window-to-buffer)))
  )

;; My synchronizer
(use-package org-msync :load-path "lisp/"
  :hook ((org-mode . org-msync-after-save-hook)
	 (auto-save . org-msync-auto-save-hook))
  :custom
  (org-msync-local-dir org-directory)
  (org-msync-remote-dir "~/Documents/org-remote/")
  )
