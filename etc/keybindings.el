;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; backward kill-line, this may not be a good idea
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/backward-kill-line (arg)
  "Kill ARG line backwards"
  (interactive "p")
  (kill-line (- 1 arg)))
(global-set-key (kbd "C-c u") 'my/backward-kill-line) ;;`C-c u'


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; move lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))
(put 'save-column 'lisp-indent-function 0)

(defun my/move-line-up ()
  (interactive)
  (save-column
    (transpose-lines 1)
    (forward-line -2)))

(defun my/move-line-down ()
  (interactive)
  (save-column
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)))

(global-set-key (kbd "M-<up>") 'my/move-line-up)
(global-set-key (kbd "M-<down>") 'my/move-line-down)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; copy current lines char
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/copy-line ()
  "copy current line, from the first character that is not \t or
  ' ', to the last of that line, this feature is from vim.
  Case to use this feature:
  - repeat similar lines in the code.
  "
  (interactive)
  (save-excursion
    (back-to-indentation)
    (let* ((beg (point))
	   (end (line-end-position))
	   (mystr (buffer-substring beg end)))
      (kill-ring-save beg end)
      (message "%s" mystr)))
  ;;This is silly, find a way to print out last-kill.
  )

(global-set-key "\C-c\C-k" 'my/copy-line)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;3) this backward-forward package helps us jump back-forward in the mark ring.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package backward-forward
  :ensure t
  :demand
  :config
  (backward-forward-mode t)
  :bind (:map backward-forward-mode-map
              ("<C-left>" . nil)
              ("<C-right>" . nil)
              ("C-c C-<left>" . backward-forward-previous-location)
              ("C-c C-<right>" . backward-forward-next-location)
              ("<mouse-8>" . backward-forward-previous-location)
              ("<mouse-9>" . backward-forward-next-location)
              )
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-mark
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package auto-mark
;;   :load-path "lisp"
;;   :config
;;   (setq auto-mark-command-class-alist
;;	'((goto-line . jump)
;;	  (undo . ignore)))
;;   (global-auto-mark-mode 1)
;;   :bind ("C-c C-<SPC>"  . 'pop-to-mark-command)
;;   )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; move to next word
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/next-word (p)
  "Move point to the beginning of the next word, past by any space"
  (interactive "d")
  (forward-word)
  (forward-word)
  (backward-word))
;;(global-set-key "\M-f" 'my/next-word)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; window move operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; insert rectangle
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(global-set-key (kbd  "\C-x r i") 'string-insert-rectangle)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reload dir-locals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun my/reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir)
          (my/reload-dir-locals-for-current-buffer))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; get current file name and full path
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/filename ()
  "Copy the filename of the current buffer."
  (interactive)
  (kill-new (buffer-name (window-buffer (minibuffer-selected-window)))))

(defun my/full-path ()
  "Copy the full path of the current buffer."
  (interactive)
  (kill-new (buffer-file-name (window-buffer (minibuffer-selected-window)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; proxy-functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my/disable-proxy ()
  "Disable the proxy used in emacs"
  (interactive)
  (setq url-proxy-services
        `(("http" . nil)
          ("https" . nil)
	  ("no_proxy" . ,(getenv "no_proxy"))))
  ;;backup the proxy settings
  (setenv "http_proxy_backup" (getenv "http_proxy"))
  (setenv "https_proxy_backup" (getenv "https_proxy"))
  (setenv "ftp_proxy_backup" (getenv "ftp_proxy"))
  ;;clean up the proxy settings
  (setenv "http_proxy" nil)
  (setenv "https_proxy" nil)
  (setenv "ftp_proxy" nil)
  )

(defun my/enable-proxy ()
  "Re-enable proxy from environment variables"
  (interactive)
  (setenv "http_proxy" (getenv "http_proxy_backup"))
  (setenv "https_proxy" (getenv "https_proxy_backup"))
  (setenv "ftp_proxy" (getenv "ftp_proxy_backup"))
  
  (setq url-proxy-services
	`(("http" . ,(getenv "http_proxy"))
	  ("https" . ,(getenv "https_proxy"))
	  ("ftp_proxy" . ,(getenv "ftp_proxy"))
	  ("no_proxy" . ,(getenv "no_proxy"))))
  )
