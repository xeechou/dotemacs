(use-package org-mode
  :ensure org
  :mode (("\\.org$" . org-mode))
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

  ;; recursively update the parents TODO
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are don, to TODO otherwise"
    (let (org-log-done org-log-states) ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
  ;;setup the default directory
  (setq org-directory "~/org/")
  (setq org-mobile-directory "~/Documents/org-remote/")

  (setq org-default-notes-file (concat org-directory "miscs.org"))
  ;; you have to set this before loading org-mode
  (setq org-agenda-files (list (concat org-directory "work.org")
			       (concat org-directory "training.org")
			       (concat org-directory "miscs.org")))
  ;; it seems if we use org-mobile-files, it is the only list we move
  (setq org-mobile-files (append org-agenda-files
				 (list (concat org-directory "notes.org")
				       (concat org-directory "today.org")
				       (concat org-directory "goals-habits.org")
				       (concat org-directory "social.org"))))

  ;; org-push
  (defun org-push-copy ()
    "copy agenda files"
    (if (file-exists-p org-mobile-directory)
	(dolist (src org-mobile-files)
	  (let (name dest)
	    (progn
	      (setq name (file-name-nondirectory src))
	      (setq dest (concat org-mobile-directory name))
	      (copy-file src dest 'ok-if-already-exists))
	    ))
      (message "%s does not exist" org-mobile-directory))
    )
  (defun org-push ()
    "org push but delete the agendas and mobileorg, use this for now"
    (interactive)
    (save-excursion
      (save-restriction
	(save-window-excursion
	  (message "Creating agendas... done")
	  (org-save-all-org-buffers)
	  (message "Coping agendas... done") ;;we will last see this if no error happens
	  (org-push-copy)
	  )))
    )
  ;; org-pull
  (defun org-pull-copy ()
    "copy agenda files"
    (if (file-exists-p org-mobile-directory)
	(dolist (dest org-mobile-files)
	  (let (name src)
	    (progn
	      (setq name (file-name-nondirectory dest))
	      (setq src (concat org-mobile-directory name))
	      (copy-file src dest 'ok-if-already-exists))
	    ))
      (message "remote %s does not exist" org-mobile-directory))
    )
  (defun org-pull ()
    "pulling all the agendas from remote, this overrides current files"
    (interactive)
    (save-excursion
      (save-restriction
	(save-window-excursion
	  (message "saving agendas... done")
	  (org-save-all-org-buffers)
	  (message "pulling agendas...done")
	  (org-pull-copy)
	  )))
    )
  ;I am not sure this global key setting is good or not, capture stuff globally is great
  :bind (:map global-map
	      ("\C-ca" . org-agenda)
	      ("\C-cc" . org-capture)
	      ("\C-cb" . org-iswitchb))
  :config

  (progn
    (setq org-log-done 'time)
    )
  )
