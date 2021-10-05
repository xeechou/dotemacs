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
(require 'json)

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

(defcustom org-msync-json "files.json"
  "json file name for caching the checksums"
  :type 'string
  :group 'org-msync)

(defvar org-msync-checksums (make-hash-table :test 'equal)
  "hash-tables to store the current file checksums")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-msync-dir-set (dir)
  (and dir (not (string= dir "")) (file-exists-p dir)))

(defun org-msync-local-json ()
  "get local json hash path"
  (concat org-msync-local-dir org-msync-json))

(defun org-msync-remote-json ()
  "get remote json hash path"
  (concat org-msync-remote-dir org-msync-json))

(defun org-msync-json-path (dir)
  "get json path using prefix"
  (concat dir org-msync-json))

(defun org-msync-entry (path dir)
  "strip path from dir prefix"
  (let ((dir (file-truename dir))
	(tpath (file-truename path)))
    (string-remove-prefix dir tpath)))

(defun org-msync-local-entry (path)
  "strip path of local org directory prefix"
  (org-msync-entry path org-msync-local-dir))

(defun org-msync-remote-entry (path)
  "strip path of remote org diretory prefix"
  (org-msync-entry path org-msync-remote-dir))


(defun org-msync-hash-file (path)
  "generate the hash for the given path"
  (with-temp-buffer
    (insert-file-contents path)
    (secure-hash 'sha1 (current-buffer))))

(defun org-msync-copy-file (entry src-dir dst-dir)
  "copy a single file to remote"
  (let* ((src-path (concat src-dir entry))
	 (dst-path (concat dst-dir entry)))
    (when (file-exists-p src-path)
      (copy-file src-path dst-path 'ok-if-already-exists))
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hash ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-msync-get-chksums (path)
  "get a hash table from path, if something exist "
  (let* ((json-object-type 'hash-table)
	 (json-array-type 'list)
	 (json-key-type 'string))
    (if (file-exists-p path)
	(json-read-file path)
      (json-read-from-string "{}"))))

(defun org-msync-write-chksums (path htable)
  "write the hash table to the path"
  (let* ((json-object-type 'hash-table)
	 (json-array-type 'list)
	 (json-key-type 'string)
	 (json-string  (json-encode htable)))
    (with-temp-buffer
      (insert json-string)
      (write-region (point-min) (point-max) path)))
  )

(defun org-msync-flush-chksums ()
  "flush our checksums to the disk"
  (when (> (hash-table-count org-msync-checksums) 0)
    ;;1. get hash table from json
    ;;2. push hash from org-msync-checksums to this json obj
    ;;3. purge the org-msync-checksums
    ;;4. encode this hash table to
    (let* ((json-local (org-msync-local-json))
	   (sums (org-msync-get-chksums json-local)))
      (maphash (lambda (k v) (puthash k v sums))
	       org-msync-checksums)
      (clrhash org-msync-checksums)
      (org-msync-write-chksums json-local sums)
      (message "flushed org checksums"))))

(defun org-msync-flush-all-local-chksums ()
  "mannually flush all checksums of local org files"
  (if (file-exists-p org-msync-local-dir)
      (let ((files (directory-files-recursively org-msync-local-dir "\.org$"))
	    (chksums (make-hash-table :test 'equal)))
	;;1: find all files and do the hash
	(dolist (src files)
	  (setq src (file-truename src))
	  (puthash (org-msync-local-entry src)
		   (org-msync-hash-file src)
		   chksums)
	  )
	;;2: write that hash to the disk
	(org-msync-write-chksums (org-msync-local-json) chksums)
	(message "flushed all org checksums")
	) ;;let
    ))

(defun org-msync-flush-all-remote-chksums ()
  "mannually updating remote checksums of org files"
  (if (file-exists-p org-msync-remote-dir)
      (let ((files (directory-files-recursively org-msync-remote-dir "\.org$"))
	    (chksums (make-hash-table :test 'equal)))
	;;1: find all files and do the hash
	(dolist (src files)
	  (setq src (file-truename src))
	  (puthash (org-msync-remote-entry src)
		   (org-msync-hash-file src)
		   chksums)
	  )
	;;2: write that hash to the disk
	(org-msync-write-chksums (org-msync-remote-json) chksums)
	(message "flushed all org checksums")
	) ;;let
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; sync ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-msync-copy (src dst)
  "copy agenda files"
  (message "Coping agendas ...")
  (if (file-exists-p dst)
      (let ((src-chksums (org-msync-get-chksums (org-msync-json-path src)))
	    (dst-chksums (org-msync-get-chksums (org-msync-json-path dst))))
	;;for every file in the checksum, copy if not equal
	(maphash (lambda (k vl)
		   (let ((vr (gethash k dst-chksums)))
		     (when (not (equal vl vr))
		       (org-msync-copy-file k src dst))))
		 src-chksums)
	(org-msync-copy-file org-msync-json src dst)
	(message "done coping from %s to %s" src dst))
    (message "%s does not exist" dst))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; interactive functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-msync-test-save-hook ()
  "print the filename and hash for current buffer"
  (let ((fname (org-msync-local-entry (buffer-file-name (current-buffer))))
	(chksum (secure-hash 'sha1 (current-buffer))))
    ;;write our hash values to the checksums, but this presents a problem, if
    ;;emacs quit before exausting the hash, we lose the data. It is best we
    ;;have a org-msync-flush-chksums function with auto-save-hook
    (puthash fname chksum org-msync-checksums)
    )
  )

;; https://stackoverflow.com/questions/6138029/how-to-add-a-hook-to-only-run-in-a-particular-mode

;;;###autoload
(defun org-msync-after-save-hook ()
  "hashing the current buffer content and push it to org-msync-chksums"
  (add-hook 'after-save-hook 'org-msync-test-save-hook nil 'make-it-local))

;;;###autoload
(defun org-msync-auto-save-hook ()
  "automatically flush the content of org-msync-chksums"
  (org-msync-flush-chksums))

;;;###autoload
(defun org-msync-pull ()
  "pulling all the agendas from remote, this overrides current files"
  (interactive)
  (save-excursion
    (save-restriction
      (save-window-excursion
	(message "saving agendas... done")
	(org-save-all-org-buffers)
	(message "writing org checksums...")
	(org-msync-flush-all-local-chksums)
	(org-msync-flush-all-remote-chksums)
	(message "coping org files...")
	(if (and (org-msync-dir-set org-msync-local-dir)
		 (org-msync-dir-set org-msync-remote-dir))
	    (org-msync-copy org-msync-remote-dir org-msync-local-dir)
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
	(message "writing org checksums...")
	(org-msync-flush-all-local-chksums)
	(message "coping org files...")
	(if (and (org-msync-dir-set org-msync-local-dir)
		 (org-msync-dir-set org-msync-remote-dir))
	    (org-msync-copy org-msync-local-dir org-msync-remote-dir)
	  (message "org-msync dirs not set"))
	)))
  )

(provide 'org-msync)
