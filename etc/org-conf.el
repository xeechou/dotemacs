(use-package org :ensure t
  :mode (("\\.org$" . org-mode))
  :custom
  (org-log-done  'time)
  (org-clock-persist 'history)
  (org-adapt-indentation nil)
  ;;agenda, show unplanned tasks in global TO-DO list.
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-todo-ignore-deadlines t)
  (org-agenda-todo-ignore-scheduled t)
  ;;faces
  (org-todo-keywords '((sequence "TODO" "DOIN" "|" "PEND" "DONE" "CANC")))
  ;;TODO, change those faces
  (org-todo-keyword-faces '(("TODO" . error)
			    ("DOIN" . org-document-title)
			    ("DONE" . org-level-5)
			    ("CANC" . org-level-4)
			    ("PEND" . org-level-3)))
  (org-hide-emphasis-markers t)
  ;;latex
  (org-latex-create-formula-image-program 'dvipng)
  (org-preview-latex-image-directory (concat temporary-file-directory "ltximg/"))
  ;;note files
  (org-default-notes-file (concat org-directory "notes.org"))
  (org-agenda-files (list (concat org-directory "reading.org")
			  (concat org-directory "writing.org")
			  (concat org-directory "coding.org")
			  (concat org-directory "social.org")
			  (concat org-directory "thoughts.org")
			  (concat org-directory "goals-habits.org")
			  (concat org-directory "miscs.org")))
  :hook (org-after-todo-statistics . org-summary-todo)
  ;I am not sure this global key setting is good or not, capture stuff globally
  ;is great
  :bind (:map global-map
	      ("\C-ca"   . org-agenda)
	      ("\C-cc"   . org-capture)
	      :map org-mode-map
	      ("M-<left>"  . org-metaleft)
	      ("M-<right>" . org-metaright)
	      ("M-<up>"    . org-metaup)
	      ("M-<down>"  . org-metadown))
  :init
  ;;set org_dir
  (defun org-dir-set (dir)
    (and dir (not (string= dir "")) (file-exists-p dir)))
  (setq org-directory (if (org-dir-set (getenv "ORG_DIR"))
			  (getenv "ORG_DIR")
			"~/org/")) ;;org-directory has to have trailing "/"
  ;; enable images
  (setq org-startup-with-inline-images t)
  ;; using org-indent-mode
  (setq org-startup-indented t)
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are don, to TODO otherwise"
    (let (org-log-done org-log-states) ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

  ;;activate babel languages
  :config
  (org-clock-persistence-insinuate)
  ;; I just use PEND to define stuck projects.
  (setq org-stuck-projects
      '("/-DONE-CANC" ("DOIN" "TODO") nil ""))
  ;;capture templates
  (setq org-capture-templates
	;; misc tasks, moving coding or writing later?
	`(("m" "Miscs" entry
	   (file+headline ,(concat org-directory "miscs.org") "Tasks")
           "* TODO %?\n%i\n  %a")
	  ;; my ideas
	  ("s" "Thoughts" entry
	   (file+headline ,(concat org-directory "thoughts.org") "Ideas")
	   "* %?\n %i\n %c\n\n")
	  ;; Learning items
	  ("r" "Reading" entry
	   (file+headline ,(concat org-directory "reading.org") "Articles")
	   "** TODO %?\n%i\n %^L\n \n") ;;why the linebreak didn't work?
	  ;; my journals
          ("j" "Journal" entry
	   (file+olp+datetree ,(concat org-directory "journal.org"))
           "* %t\n %? %i\n")
	  ("p" "Review+Planning" entry
	   (file+headline ,(concat org-directory "writing.org") "Review+Planning")
	   "** On %t\n*** Review:\n- %? \n*** Planned:\n\n %i \n ")
	  ))

  (let ((has_python (if (executable-find "python") t nil))
	(has_c (if (and (executable-find "gcc") (executable-find "g++")) t nil))
	)
    (org-babel-do-load-languages
     'org-babel-load-languages
     `((emacs-lisp  . t)
       (shell       . t)
       (python      . ,has_python)
       (C           . ,has_c)
       ))
    )
  ;;face settings, well setting different sizes for levels is Well, I am not
  ;;very sure, need to be in the same color, didn't look as pretty as I
  ;;expected, and it breaks the TODOs.

  ;; (set-face-attribute 'org-level-1 nil :height 1.5 :weight 'bold)
  ;; (set-face-attribute 'org-level-2 nil :height 1.25 :weight 'bold)
  ;; (set-face-attribute 'org-level-3 nil :height 1.1 :weight 'bold)
  ;; (set-face-attribute 'org-level-4 nil :height 1.05 :weight 'bold)
  )

;; org-roam minor mode
(use-package org-roam
  :ensure t
  :after org
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

;; using org bullets
(use-package org-bullets
  :ensure t
  :after org
  :hook ((org-mode . org-bullets-mode)))

;; org clip link
(use-package org-cliplink
  :ensure t
  :bind ("C-c p i" . org-cliplink))

;; Org-download
(use-package org-download
  :if window-system
  :ensure t
  :after org
  ;; :custom (org-download-image-dir (concat org-directory "imgs/"))
  ;; this hook will run twice because of org-clock
  ;; :hook (org-mode . (lambda ()
  ;;		      (message "buffer file name %s" (buffer-file-name))
  ;;		      ;;only set download-image-dir for org-dir and roam-dir
  ;;		      (let* ((curdir (file-name-directory (buffer-file-name)))
  ;;			     (orgdir org-directory)
  ;;			     (roamdir (concat orgdir "roam/")))
  ;;			(setq org-download-image-dir
  ;;			      (if (and (not (boundp 'org-download-image-dir))
  ;;				       (or (string= curdir orgdir)
  ;;					   (string= curdir roamdir)))
  ;;				  (concat curdir "imgs/")
  ;;				nil)))
  ;;		      ))
  :bind (:map org-mode-map
	      (("C-c d s" . org-download-screenshot)
	       ("C-c d y" . org-download-yank)
	       ("C-c d c" . org-download-clipboard))))

(use-package ivy-bibtex
  :ensure t
  :after org
  :init
  (setq bibtex-completion-bibliography `,(concat org-directory "bib/references.bib")))

(use-package org-ref
  :ensure t
  :after org
  :init
  (require 'org-ref-arxiv)
  (require 'org-ref-scopus)
  (require 'org-ref-wos)
  (require 'org-ref-ivy)
  (setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
	org-ref-insert-cite-function 'org-ref-cite-insert-ivy
	org-ref-insert-label-function 'org-ref-insert-label-link
	org-ref-insert-ref-function 'org-ref-insert-ref-link
	org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body)))
  ;; setup auto generating bibtex keys
  (require 'bibtex)
  (setq bibtex-autokey-year-length 4
	bibtex-autokey-name-year-separator "-"
	bibtex-autokey-year-title-separator "-"
	bibtex-autokey-titleword-separator "-"
	bibtex-autokey-titlewords 2
	bibtex-autokey-titlewords-stretch 1
	bibtex-autokey-titleword-length 5)
  ;; export to pdf with bibtex
  ;;this is when you don't have latexmk
  (setq org-latex-pdf-process
	(if (executable-find "latexmk")
	    ;;when you have latexmk
	    (list "latexmk -shell-escape -bibtex -f -pdf %f")
	  ;;when you don't have latexmk
	  '("pdflatex -interaction nonstopmode -output-directory %o %f"
	    "bibtex %b" ;;using bibtex here, or you can use biber
	    "pdflatex -interaction nonstopmode -output-directory %o %f"
	    "pdflatex -interaction nonstopmode -output-directory %o %f")))

  :bind (:map org-mode-map
	      ("C-c [" . org-ref-insert-link-hydra/body)
	      ("C-c ]" . org-ref-insert-link))
  )
