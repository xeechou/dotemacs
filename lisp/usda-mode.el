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

;; code mostly took from https://www.omarpolo.com/post/writing-a-major-mode.html
(defun usda-indent-line ()
  "Indent current line."
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
(define-derived-mode usda-mode c-mode "USDa"
  "Major mode for editing USD's ascii representation"
  (setq font-lock-defaults '((usda-font-lock-keywords)))
  ;; comments
  (setq-local comment-start "#")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#+[\t ]*")
  (setq-local indent-line-function #'usda-indent-line)
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 4)
  )

(provide 'usda-mode)
