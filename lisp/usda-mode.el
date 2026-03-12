;; usage: load this at emacs start
;; m-x usda-mode
;; see LICENSE for details
;; github.com/superfunc/usda-syntax

(setq usda-font-lock-keywords
      (let* ((usda-keywords '("def" "class" "over" "payload" "subLayers"
			      "references" "inherits" "variants"
			      "variantSet" "variantSets"
			      "dictionary" "clips" "customData"
			      "uniform" "custom" "timeSamples"))
	     (usda-types '("double" "double2" "double3" "double4"
			   "float" "float2" "float3" "float4"
			   "half" "half2" "half3" "half4"
			   "int" "int2" "int3" "int4"
			   "uchar" "uchar2" "uchar3" "uchar4"
			   "uint" "uint2" "uint3" "uint4"
			   "vector3d" "vector3f" "vector3h"
			   "normal3d" "normal3f" "normal3h"
			   "point3d" "point3f" "point3h"
			   "color3d" "color3f" "color3h"
			   "color4d" "color4f" "color4h"
			   "matrix2d" "matrix3d" "matrix4d"
			   "quatd" "quatf" "quath"
			   "quatd[]" "quatf[]" "quath[]"
			   "token" "string" "asset" "frame4d"
			   "add" "remove" "reorder"))
	     (usda-special-names '("kind" "defaultPrim" "upAxis"
				   "startTimeCode" "endTimeCode"
				   "instanceable" "hidden" "active"))
	     (usda-comments-regexp "#.*")
	     (usda-keywords-regexp (regexp-opt usda-keywords 'words))
	     (usda-types-regexp (regexp-opt usda-types 'words))
	     (usda-assetp-regexp "\@")
	     (usda-path-open-regexp "\<\/")
	     (usda-path-close-regexp "\>")
	     (usda-special-regexp (regexp-opt usda-special-names 'words))
	     (usda-num-regexp (regexp-opt '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "."))))

	`((,usda-types-regexp . font-lock-type-face)
	  (,usda-keywords-regexp . font-lock-keyword-face)
	  (,usda-special-regexp . font-lock-builtin-face)
	  (,usda-num-regexp . font-lock-constant-face)
	  (,usda-path-open-regexp . font-lock-constant-face)
	  (,usda-path-close-regexp . font-lock-constant-face)
	  (,usda-assetp-regexp . font-lock-constant-face)
	  (,usda-comments-regexp . font-lock-comment-face))))

(defvar usda-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Make " punctuation ("." class) by default so it doesn't
    ;; auto-start strings.  We handle all string delimiters via
    ;; syntax-propertize instead.
    (modify-syntax-entry ?\" "." table)
    ;; # starts a line comment ("<" = comment-starter)
    (modify-syntax-entry ?# "<" table)
    ;; newline ends a line comment (">" = comment-ender)
    (modify-syntax-entry ?\n ">" table)
    ;; Paired delimiters for paren-matching, indentation, and sexp navigation.
    ;; "(" class = open paren, ")" class = close paren;
    ;; the second character is the matching counterpart.
    (modify-syntax-entry ?\( "()" table)   ; ( matches )
    (modify-syntax-entry ?\) ")(" table)   ; ) matches (
    (modify-syntax-entry ?\{ "(}" table)   ; { matches }
    (modify-syntax-entry ?\} "){" table)   ; } matches {
    (modify-syntax-entry ?\[ "(]" table)   ; [ matches ]
    (modify-syntax-entry ?\] ")[" table)   ; ] matches [
    table)
  "Syntax table for `usda-mode'.")

(defvar usda-syntax-propertize-function
  (syntax-propertize-rules
   ;; Triple-quoted strings: """..."""
   ("\"\"\"" (0 (ignore (usda-syntax-stringify))))
   ;; Regular double-quoted strings: "..."
   ("\"" (0 (ignore (usda-syntax-stringify-single)))))
  "Syntax propertize rules for strings in USDA.")

(defun usda-syntax-stringify ()
  "Put `syntax-table' property on triple-quoted strings."
  (let* ((ppss (save-excursion
                 (backward-char 3)
                 (syntax-ppss)))
         (in-string (nth 3 ppss)))
    (if in-string
        ;; We're closing a triple-quoted string.
        ;; The last quote of the closing """ gets string-fence (|).
        (put-text-property (1- (point)) (point)
                           'syntax-table (string-to-syntax "|"))
      ;; We're opening a triple-quoted string.
      ;; The first quote of the opening """ gets string-fence (|).
      (put-text-property (- (point) 3) (- (point) 2)
                         'syntax-table (string-to-syntax "|")))))

(defun usda-syntax-stringify-single ()
  "Put `syntax-table' property on regular double-quoted strings.
Only applies when not already inside a triple-quoted string."
  (let* ((ppss (save-excursion
                 (backward-char 1)
                 (syntax-ppss)))
         (in-string (nth 3 ppss)))
    (unless (eq in-string t)  ; not inside a string-fence (triple-quoted) string
      ;; Mark this quote as a regular string delimiter
      (put-text-property (1- (point)) (point)
                         'syntax-table (string-to-syntax "\"")))))

;; code mostly took from https://www.omarpolo.com/post/writing-a-major-mode.html
(defun usda-indent-line ()
  "Indent current line.

Algorithm:
1. Move to the first non-whitespace character on the line.
2. Use `syntax-ppss' to get the paren/brace/bracket nesting depth
   at that position — this becomes the base indentation level.
3. If the line is empty and the cursor wasn't at the indentation
   point, don't indent (set level to 0).
4. If the first non-whitespace character is a closing delimiter
   (`)' `}' `]'), subtract 1 from the level so it aligns with
   its matching opener rather than the content inside.
5. Replace all leading whitespace with (level × tab-width) spaces.
6. If the cursor was at the indentation point when invoked, move
   it to the end of line after re-indenting."
  (let (indent
	boi-p                           ;begin of indent
	move-eol-p
	(point (point)))                ;lisps-2 are truly wonderful
    (save-excursion
      (back-to-indentation)
      ;;(car (syntax-ppss)) gives depth we are in parenthesis
      (setq indent (car (syntax-ppss))
	    boi-p (= point (point)))
      ;; don't indent empty lines if they don't have the in it
      (when (and (eq (char-after) ?\n)
		 (not boi-p))
	(setq indent 0))
      ;; check whether we want to move to the end of line
      (when boi-p
	(setq move-eol-p t))
      ;; decrement the indent if the first character on the line is a
      ;; closer.
      (when (or (eq (char-after) ?\))
		(eq (char-after) ?\})
		(eq (char-after) ?\]))
	(setq indent (1- indent)))
      ;; indent the line
      (delete-region (line-beginning-position)
		     (point))
      (indent-to (* tab-width indent)))
    (when move-eol-p
      (move-end-of-line nil))))


;;;###autoload
(progn
  (add-to-list 'auto-mode-alist '("\\.usda\\'" . usda-mode))
  (add-to-list 'auto-mode-alist '("\\.usd\\'" . usda-mode)))

;;;###autoload
(define-derived-mode usda-mode prog-mode "USDa"
  "Major mode for editing USD's ascii representation."
  :syntax-table usda-mode-syntax-table
  (setq font-lock-defaults '((usda-font-lock-keywords)))
  ;; comments
  (setq-local comment-start "#")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+[\t ]*")
  (setq-local indent-line-function #'usda-indent-line)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 4)
  (setq-local syntax-propertize-function usda-syntax-propertize-function))

(provide 'usda-mode)
