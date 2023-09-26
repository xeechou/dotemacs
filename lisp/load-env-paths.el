(provide 'load-env-paths)


;;;;;;;;;;;; load user config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;add any additional environment variable like GIT_PATH RUST_PATH to PATH and
;;exec-path
(defun load-env-paths ()
  "add any additional environment variable like GIT_PATH RUST_PATH to PATH and exec-path"
  (interactive)

  (let ((additional-path "")
	(path-list (exec-path)))
    (progn
      (dolist (e process-environment)
	(when (string-match "^.+_PATH=" e)
	  ;;(car (cdr list)) :gets the second element
	  (let* ((path (car (cdr (split-string e "="))))
		 ;;posix path
		 (posix-path (replace-regexp-in-string "\\\\" "/" path)))
	    ;; if posix-path is not in path-list
	    (unless (member posix-path path-list)
	      (setq additional-path
		    (concat path path-separator additional-path))
	      (add-to-list 'path-list posix-path)
	      (message "load-env-paths: add additional path %s" posix-path)
	      ))))
      (setenv "PATH" (concat additional-path (getenv "PATH")))
      (setq exec-path path-list)))
  )
