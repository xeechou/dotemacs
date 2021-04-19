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
  (setq org-preview-latex-image-directory "/tmp/ltximg/")

  ;; recursively update the parents TODO
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are don, to TODO otherwise"
    (let (org-log-done org-log-states) ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
  ;;setup the default directory
  (setq org-mobile-dir "~/Documents/org-remote/")

  (setq org-default-notes-file (concat org-directory "miscs.org"))
  ;; you have to set this before loading org-mode
  (setq org-agenda-files (list (concat org-directory "work.org")
			       (concat org-directory "training.org")
			       (concat org-directory "miscs.org")
			       (concat org-directory "goals-habits.org")))
  ;; it seems if we use org-mobile-files, it is the only list we move
  (setq org-mobile-files (append org-agenda-files
				 (list (concat org-directory "notes.org")
				       (concat org-directory "journal.org")
				       (concat org-directory "today.org")
				       (concat org-directory "social.org"))))
  (setq org-log-done 'time)

  ;; org-push
  (defun org-push-copy ()
    "copy agenda files"
    (message "Coping agendas ...")
    (if (file-exists-p org-mobile-dir)
	(let ((files (directory-files-recursively org-directory "\.org$")))
	  (progn (dolist (src files)
		   (let (name dest)
		     (progn
		       (setq name (string-remove-prefix org-directory src))
		       (setq dest (concat org-mobile-dir name))
		       (copy-file src dest 'ok-if-already-exists))))
		 (message "Coping agendas done")
		 )
	  )
      (message "%s does not exist" org-mobile-dir))
    )
  (defun org-push ()
    "org push but delete the agendas and mobileorg, use this for now"
    (interactive)
    (save-excursion
      (save-restriction
	(save-window-excursion
	  (message "Creating agendas... done")
	  (org-save-all-org-buffers)
	  (org-push-copy)
	  )))
    )
  ;; org-pull
  (defun org-pull-copy ()
    "copy agenda files"
    (message "pulling orgs...")
    (if (file-exists-p org-mobile-dir)
	(let ((files (directory-files-recursively org-mobile-dir "\.org$")))
	  ;;coping from remote to local
	  (progn (dolist (src files)
		   (let (name dest)
		     (setq name (string-remove-prefix org-mobile-dir src))
		     (setq dest (concat org-directory name))
		     (copy-file src dest 'ok-if-already-exists)
		     ))
		 (message "pulling orgs... done"))
	  )
      (message "remote %s does not exist" org-mobile-dir))
    )
  (defun org-pull ()
    "pulling all the agendas from remote, this overrides current files"
    (interactive)
    (save-excursion
      (save-restriction
	(save-window-excursion
	  (message "saving agendas... done")
	  (org-save-all-org-buffers)
	  (org-pull-copy)
	  )))
    )
  ;I am not sure this global key setting is good or not, capture stuff globally
  ;is great
  :bind (:map global-map
	      ("\C-ca" . org-agenda)
	      ("\C-cc" . org-capture)
	      ("\C-cb" . org-iswitchb))
  )

;; org-roam minor mode
(use-package org-roam
  :ensure t
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory (concat org-directory "roam/"))
  :bind (:map org-roam-mode-map
              (("C-c n r" . org-roam-buffer-toggle-display) ;;toggle-back-links
               ("C-c n f" . org-roam-find-file)
               ("C-c n d" . org-roam-find-directory)
               ("C-c n g" . org-roam-graph))
              :map org-mode-map
              (("C-c n i" . org-roam-insert))
              (("C-c n I" . org-roam-insert-immediate)))
  :config
  (when (eq system-type 'windows-nt)
    (setq org-roam-db-update-method 'immediate))
  (setq org-roam-capture-templates
        '(
          ("d" "default" plain #'org-roam-capture--get-point
           :file-name "%<%Y-%m-%d-%H-%M-%S>-${slug}"
           :head "#+title: ${title}\n#+ROAM_TAGS: %^{org-roam-tags}\n\
#+created: %u\n#+last_modified: %U\n%?"
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
  (setq org-roam-completion-system 'ivy)
  )
