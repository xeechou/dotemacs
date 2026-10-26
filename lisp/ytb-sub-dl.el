;;; ytb-sub-dl.el --- downloading youtube transcript and insert to buffer

(require 'dash)
(require 'dom)
(require 'xml)

;; Courtesy of: https://sachachua.com/blog/2021/04/org-mode-insert-youtube-video-with-separate-captions/

(defun ytb-sub-dl--msecs-to-timestamp (msecs)
  "Convert MSECS to string in the format HH:MM:SS.MS."
  (concat (format-seconds "%02h:%02m:%02s" (/ msecs 1000))
          "." (format "%03d" (mod msecs 1000))))

(defconst ytb-sub-dl--ytb-header-fmt "#+begin_export html
<iframe width=\"560\" height=\"315\" src=\"https://www.youtube.com/embed/%s\" title=\"YouTube video player\" frameborder=\"0\" allow=\"accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture\" allowfullscreen></iframe>\n#+end_export\n")

(defun ytb-sub-dl--ytb-header (id &optional use-header)
  "Return YouTube header when required"
  (if use-header (format ytb-sub-dl--ytb-header-fmt id) ""))

(defun ytb-sub-dl--ytb-line (o id &optional use-timestamp)
  "return the transcript line with optional timestamps"
  (let ((fmt-with-timestamp "| %s | [[https://youtube.com/watch?v=%s&t=%ss][%s]] |\n")
        (fmt-without-timestamp "%s\n"))
    (format (if use-timestamp fmt-with-timestamp fmt-without-timestamp)
            ;; the text itself, with regexp replacing
            (->> (dom-text o)
                 (replace-regexp-in-string "[ \n]+" " ")
                 (replace-regexp-in-string "&#39;" "'")
                 (replace-regexp-in-string "&quot;" "\""))
            ;; the rest
            id
            (dom-attr o 'start)
            (ytb-sub-dl--msecs-to-timestamp (* 1000 (string-to-number (dom-attr o 'start)))))))

;;;###autoload
(defun ytb-sub-dl-insert-transcript (url &optional langs use-header use-timestamp)
  "Insert the transcript of the YouTube video at URL into the current buffer.
LANGS is the subtitle language(s) for yt-dlp's --sub-lang option,
e.g. \"en\", \"zh\", or a comma-separated priority list such as
\"en-orig,en\" (the default).  With USE-HEADER, also insert an
embedded video player header.  With USE-TIMESTAMP, format each
line as an org table row with a link to that point in the video."
  (interactive "MURL: ")
  (let* ((id (if (string-match "v=\\([^&]+\\)" url) (match-string 1 url) url))
         (langs (or langs "en-orig,en"))
         (temp-file (make-temp-name
                     (expand-file-name "org-youtube-" temporary-file-directory)))
         (errbuf (get-buffer-create "*ytb-sub-dl*"))
         (sub-files (-map (lambda (lang) (concat temp-file "." lang ".srv1"))
                           (split-string langs "," t)))
         temp-file-name)
    (with-current-buffer errbuf (erase-buffer))
    (call-process "yt-dlp" nil errbuf nil
                  "--write-sub" "--write-auto-sub" "--no-warnings"
                  "--sub-lang" langs "--skip-download" "--sub-format" "srv1"
                  "-o" temp-file
                  (format "https://youtube.com/watch?v=%s" id))
    ;; yt-dlp can exit non-zero when a lower-priority language fails
    ;; (e.g. HTTP 429 on the plain "en" track) even though the primary
    ;; subtitle was written, so go by the file instead of the exit code.
    ;; The file is named after the language actually fetched, e.g.
    ;; "...en-orig.srv1", hence the search over the requested languages.
    (setq temp-file-name (-first #'file-exists-p sub-files))
    (if temp-file-name
        (progn
          (insert
           (ytb-sub-dl--ytb-header id use-header)
           (if use-header "\n" "") ;;additional line break when insert header
           (mapconcat (lambda (o) (ytb-sub-dl--ytb-line o id use-timestamp))
                      ;;return the list of text element in xml, will be something
                      ;;like this:
                      ;;
                      ;; ( (text ((start . "0.24") (dur . "3.96")) "the text") ... ))
                      (dom-by-tag (xml-parse-file temp-file-name) 'text)
                      ""))
          (dolist (f sub-files)
            (when (file-exists-p f) (delete-file f))))
      (error "Could not download %s subtitles for %s, see *ytb-sub-dl* buffer for yt-dlp output"
             langs id))))

(provide 'ytb-sub-dl)
