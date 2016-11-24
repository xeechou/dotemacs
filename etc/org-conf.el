(use-package org-mode
  :ensure org
  :mode (("\\.org$" . org-mode))
  :init
  (setq org-todo-keywords
	'((sequence "TODO" "DOIN" "PEND" "|" "DONE" "CANC")))
  (setq org-todo-keyword-faces
	'(("TODO" . error)
	  ("DOIN" . org-document-title)
	  ("DONE" . org-level-5)
	  ("CANC" . org-level-4)
	  ("PEND" . org-level-3)))
  
  :config
  (progn
    (setq org-log-done t)
    (setq org-agenda-files (list "~/org/work.org"
				 "~/org/training.org"
				 "~/org/social.org"))

    )
  
  )
