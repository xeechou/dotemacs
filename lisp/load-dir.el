;; 2016-3-23, the simple but good enough load-dir function.
;; I won't run into the argument error anymore



(provide 'load-dir)


(defun load-dir (dir)
  "load-lisp files from a directory"
  (interactive)
  (let* ((expanded-dir (expand-file-name (substitute-in-file-name dir)))
	 (files (directory-files expanded-dir)))
    (message "load-dir: files are %s" files)
    (while files
      (let* ((file (car files))
	     (abs-file (concat expanded-dir "/" file)))

	(when (string-match "\.el$" file)
	  (load-file abs-file)))
      (setq files (cdr files)))
    )
  )
