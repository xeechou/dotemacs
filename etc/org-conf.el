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
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)

  (setq org-default-notes-file (concat org-directory "/notes.org"))

  ;; recursively update the parents TODO
  (defun org-summary-todo (n-done n-not-done)
    "Switch entry to DONE when all subentries are don, to TODO otherwise"
    (let (org-log-done org-log-states) ; turn off logging
      (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

  ;;you have to set this before loading org-mode, really weird
  (setq org-agenda-files (list "~/org/work.org"
			       "~/org/training.org"
			       "~/org/miscs.org"
			       "~/org/today.org"))

;  (define-key global-map "\C-cc" 'org-capture)

  ;I am not sure this global key setting is good or not, capture stuff globally is great
  (global-set-key "\C-ca" 'org-agenda)
  (global-set-key "\C-cc" 'org-capture)
  (global-set-key "\C-cb" 'org-iswitchb)
  :config
  (progn
    (setq org-log-done 'time)
    )
  )
