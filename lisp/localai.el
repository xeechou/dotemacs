;;; localai.el --- LocalAI utilities -*- lexical-binding: t; -*-

;; Simple utility to fetch model ids from a LocalAI-compatible server
;; Place this file in your load-path (e.g. ~/.emacs.d/lisp/) and
;; (require 'localai) to use the functions below.

;;; Commentary:
;; Provides two main functions:
;; - `localai-get-models-sync'  : synchronous fetch, returns list of id strings
;; - `localai-get-models-async' : asynchronous fetch, calls a callback with the ids
;;
;; Both functions default to querying
;; "https://localai.zhumi.casa/v1/models" but accept an optional URL
;; argument for reuse.

;;; Code:

(require 'json)
(require 'url)

(defgroup localai nil
  "Configuration group for LocalAI utilities."
  :group 'applications)

(defcustom localai-default-url "https://localai.zhumi.casa"
  "Default base URL for the LocalAI server (no endpoint path).
The models functions append \"/v1/models\" to this base URL when
querying available models."
  :type 'string
  :group 'localai)

(defun localai--parse-buffer-for-data ()
  "Assume point is at beginning of buffer containing an HTTP response.
Skip headers and parse the JSON body returning the parsed object.
Returns nil on parse error." 
  (when (re-search-forward "\r?\n\r?\n" nil 'move)
    (let ((json-object-type 'alist)
          (json-array-type 'list))
      (condition-case err
          (json-parse-buffer :object-type 'alist :array-type 'list)
        (error
         (message "localai: JSON parse error: %s" err)
         nil)))))

(defun localai--extract-ids (parsed-json)
  "Return a list of model id strings from PARSED-JSON.
Handles the common shape where the top-level has a \"data\"
array of objects with an \"id\" key. Returns nil if nothing found."
  (when parsed-json
    (let ((items (cond
                  ((hash-table-p parsed-json)
                   (gethash "data" parsed-json))
                  ((and (listp parsed-json) (assoc "data" parsed-json))
                   (alist-get "data" parsed-json nil nil #'string=)
                   )
                  ((and (listp parsed-json) (assoc 'data parsed-json))
                   (alist-get 'data parsed-json))
                  (t nil))))
      (when (listp items)
        (delq nil (mapcar (lambda (m)
                            (cond
                             ((and (listp m) (assoc "id" m))
                              (alist-get "id" m nil nil #'string=))
                             ((and (listp m) (assoc 'id m))
                              (alist-get 'id m))
                             (t nil)))
                          items))))))

;;;###autoload
(defun localai-get-models-sync (&optional base-url timeout)
  "Synchronously fetch model ids from BASE-URL (defaults to `localai-default-url`).
BASE-URL should be the server base (no endpoint path). This function will
query the /v1/models endpoint on the server. Returns a list of id strings
or nil on error. If called interactively, prints the list in the echo area.
TIMEOUT is the number of seconds to wait for a response (default 10)."
  (interactive)
  (let* ((base (or base-url localai-default-url))
         (endpoint (if (string-suffix-p "/" base)
                       (concat (substring base 0 -1) "/v1/models")
                     (concat base "/v1/models")))
         (timeout (or timeout 10))
         (url-request-extra-headers '(("Accept" . "application/json"))))
    (let ((buf (url-retrieve-synchronously endpoint t t timeout)))
      (if (not buf)
          (progn
            (when (called-interactively-p 'any)
              (message "localai: request to %s failed or timed out" endpoint))
            nil)
        (unwind-protect
            (with-current-buffer buf
              (goto-char (point-min))
              (let ((parsed (localai--parse-buffer-for-data)))
                (let ((ids (localai--extract-ids parsed)))
                  (when (called-interactively-p 'any)
                    (message "localai: %S" ids))
                  ids)))
          (when (buffer-live-p buf)
            (kill-buffer buf)))))))

;;;###autoload
(defun localai-get-models-async (callback &optional base-url)
  "Asynchronously fetch model ids from BASE-URL (defaults to `localai-default-url`).
BASE-URL should be the server base (no endpoint path). CALLBACK is a
function of one argument which will be called with the list of ids (or
nil on error).
Example:
  (localai-get-models-async (lambda (ids) (message \"%S\" ids)))"
  (interactive (list (lambda (ids) (message "localai: %S" ids))))
  (let* ((base (or base-url localai-default-url))
         (endpoint (if (string-suffix-p "/" base)
                       (concat (substring base 0 -1) "/v1/models")
                     (concat base "/v1/models"))))
    (let ((url-request-extra-headers '(("Accept" . "application/json"))))
      (url-retrieve
       endpoint
       (lambda (status)
         (unwind-protect
             (let ((err (plist-get status :error)))
               (if err
                   (progn
                     (message "localai: request error: %S" err)
                     (condition-case _e
                         (funcall callback nil)
                       (error nil)))
                 (goto-char (point-min))
                 (let ((parsed (localai--parse-buffer-for-data)))
                   (let ((ids (localai--extract-ids parsed)))
                     (condition-case _e
                         (funcall callback ids)
                       (error (funcall callback nil)))))))
           (when (buffer-live-p (current-buffer))
             (kill-buffer (current-buffer)))))))))

(provide 'localai)

;;; localai.el ends here
