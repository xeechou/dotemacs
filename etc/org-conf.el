;;need this to avoid issue
(straight-use-package 'org)

(require 'org-funcs)
(defun my/org-dir-set (dir)
  (and dir (not (string= dir "")) (file-exists-p dir)))
(defun my/org-file (path)
  (my/concat-path org-directory path))

(use-package org
  :ensure t
  :mode (("\\.org$" . org-mode))
  :custom
  (org-log-done  'time)
  (org-clock-persist 'history)
  (org-adapt-indentation nil)
  ;;setup the column, this max length for the first level we can go, maybe we
  ;;can somehow calculate it?
  (org-tags-column -54)
  ;;agenda, show unplanned tasks in global TO-DO list.
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-todo-ignore-deadlines t)
  (org-agenda-todo-ignore-scheduled t)
  (org-deadline-warning-days 7)
  ;;faces
  (org-todo-keywords '((sequence "TODO" "DOIN" "|" "DONE" "PEND" "CANC")))
  ;;TODO, change those faces

  (org-hide-emphasis-markers t)
  ;;latex
  (org-latex-create-formula-image-program 'dvipng)
  (org-preview-latex-image-directory (concat temporary-file-directory "ltximg/"))
  ;;note files
  (org-default-notes-file (my/concat-path org-directory "notes.org"))
  (org-agenda-files (list (my/concat-path org-directory "reading.org")
			  (my/concat-path org-directory "writing.org")
			  (my/concat-path org-directory "coding.org")
			  (my/concat-path org-directory "social.org")
			  (my/concat-path org-directory "thoughts.org")
			  (my/concat-path org-directory "goals-habits.org")
			  (my/concat-path org-directory "miscs.org")))
  :hook
  ((org-after-todo-statistics . org-funcs-summary-todo)
   (org-checkbox-statistics . org-funcs-checkbox-todo))

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
  (setq org-directory (if (my/org-dir-set (getenv "ORG_DIR"))
			  (getenv "ORG_DIR")
			"~/org/")) ;;org-directory has to have trailing "/"
  ;; enable images
  (setq org-startup-with-inline-images t)
  ;; using org-indent-mode
  (setq org-startup-indented t)
  ;; using fixed pitch for tables
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)

  ;;activate babel languages
  :config
  (org-clock-persistence-insinuate)
  ;; I just use PEND to define stuck projects.
  (setq org-stuck-projects
	'("/-DONE-CANC" ("DOIN" "TODO") nil ""))
  ;;set latex preview scale
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  ;;capture templates
  (setq org-capture-templates
	;; misc tasks, moving coding or writing later?
	`(("m" "Miscs" entry
	   (file+headline ,(my/org-file "miscs.org") "Tasks")
           "* TODO %?\n%i\n  %a")
	  ;; my ideas
	  ("s" "Thoughts" entry
	   (file+headline ,(my/org-file "thoughts.org") "Ideas")
	   "* %?\n %i\n %c\n\n")
	  ;; Learning items
	  ("r" "Reading" entry
	   (file+headline ,(my/org-file "reading.org") "Articles")
	   "** TODO %?\n%i\n %^L\n \n") ;;why the linebreak didn't work?
	  ;; my journals
          ("j" "Journal" entry
	   (file+olp+datetree ,(my/org-file "journal.org"))
           "* %t\n %? %i\n")
	  ("p" "Review+Planning" entry
	   (file+headline ,(my/org-file "goals-habits.org") "Review+Planning")
	   "*** On %t\n**** Planned:\n\n %i \n ")
	  ))
  (org-funcs-load-babel-compiler)
  (org-funcs-define-faces)
  ;;face settings, well setting different sizes for levels is Well, I am not
  ;;very sure, need to be in the same color, didn't look as pretty as I
  ;;expected, and it breaks the TODOs.
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-roam
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-roam
  :ensure t
  :after org
  :init (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (my/org-file "roam/"))
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
;; (use-package org-msync :load-path "lisp/"
;;   :hook ((org-mode . org-msync-after-save-hook)
;; 	 (auto-save . org-msync-auto-save-hook))
;;   :custom
;;   (org-msync-local-dir org-directory)
;;   (org-msync-remote-dir "~/Documents/org-remote/")
;;   )

;; using org bullets
(use-package org-bullets
  :ensure t
  :after org
  :hook ((org-mode . org-bullets-mode)))

;; org clip link
(use-package org-cliplink
  :ensure t
  :bind (:map org-mode-map
	 ("C-c p i" . org-cliplink)
	 ("C-c p l" . org-store-link)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-roam-ui
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package org-roam-ui
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :diminish org-roam-ui-mode
  :after org-roam
  :hook (after-init . org-roam-ui-mode)
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-ref
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (setq bibtex-completion-bibliography `,(my/org-file "bib/references.bib")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-ref
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

