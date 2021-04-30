;;; org-msync.el my org file synchronizer


;;; Commentary:

;; org-msync is my small synchonizer, it works my copy org files from one dir
;; to another, right now it is very naive. We will need to extend it later to
;; make it faster

;; TODO
;; 1. writing small hooks to get md5/sha1 on save an org-file;
;; 2. Using json or simply a file-list to store the file name and md5/sha1
;; 3. When transmitting. comparing the md5/sha1, only copy files that has
;;    changed.

(require 'org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup org-msync nil
  "My Org sync"
  :group 'org-msync)

(defcustom org-msync-local-dir ""
  "The local dir we store our org files"
  :type 'string
  :group 'org-msync)

(defcustom org-msync-remote-dir ""
  "The remove dir we store the org files"
  :type 'string
  :group 'org-msync)


;; https://stackoverflow.com/questions/6138029/how-to-add-a-hook-to-only-run-in-a-particular-mode
(defcustom org-msync-save-hook '()
  "The synchronizer hook to "
  :type 'hook
  :group 'org-msync)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; code ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-msync-push-copy (local remote)
  "copy agenda files"
  (message "Coping agendas ...")
  (if (file-exists-p remote)
      (let ((files (directory-files-recursively local "\.org$")))
	(progn (dolist (src files)
		 (let (name dest)
		   (progn
		     (setq name (string-remove-prefix local src))
		     (setq dest (concat remote name))
		     (copy-file src dest 'ok-if-already-exists))))
	       (message "Coping agendas done")
	       )
	)
    (message "%s does not exist" remote))
  )

(defun org-msync-pull-copy (local remote)
  "copy agenda files"
  (message "pulling orgs...")
  (if (file-exists-p remote)
      (let ((files (directory-files-recursively remote "\.org$")))
	;;coping from remote to local
	(progn (dolist (src files)
		 (let (name dest)
		   (setq name (string-remove-prefix remote src))
		   (setq dest (concat local name))
		   (copy-file src dest 'ok-if-already-exists)
		   ))
	       (message "pulling orgs... done"))
	)
    (message "remote %s does not exist" remote))
  )

(defun org-msync-dir-set (dir)
  (and dir (not (string= dir "")) (file-exists-p dir)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interactive functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun org-msync-pull ()
  "pulling all the agendas from remote, this overrides current files"
  (interactive)
  (save-excursion
    (save-restriction
      (save-window-excursion
	(message "saving agendas... done")
	(org-save-all-org-buffers)
	(if (and (org-msync-dir-set org-msync-local-dir)
		 (org-msync-dir-set org-msync-remote-dir))
	    (org-msync-pull-copy org-msync-local-dir org-msync-remote-dir)
	  (message "org-msync dirs not set"))
	)))
  )

;;;###autoload
(defun org-msync-push ()
  "org push but delete the agendas and mobileorg, use this for now"
  (interactive)
  (save-excursion
    (save-restriction
      (save-window-excursion
	(message "Creating agendas... done")
	(org-save-all-org-buffers)
	(if (and (org-msync-dir-set org-msync-local-dir)
		 (org-msync-dir-set org-msync-remote-dir))
	    (org-msync-push-copy org-msync-local-dir org-msync-remote-dir)
	  (message "org-msync dirs not set"))
	)))
  )

(provide 'org-msync)
