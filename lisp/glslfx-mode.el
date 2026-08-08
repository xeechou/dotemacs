;;; glslfx-mode.el --- Major mode for GLSLFX shader files -*- lexical-binding: t -*-

;; Copyright (C) 2026 Xichen Zhou
;; Author: Xichen Zhou <sichem.zh@gmail.com>
;; Maintainer: Xichen Zhou <sichem.zh@gmail.com>
;; Keywords: languages OpenGL GLSL USD Hydra
;; Package-Requires: ((emacs "26.1") (glsl-mode "2.7.1"))
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `glslfx-mode' is a thin wrapper around `glsl-mode' for Pixar/OpenUSD
;; GLSLFX shader container files.  GLSLFX files contain GLSL snippets plus
;; container directives such as `-- glsl', `-- layout', `-- configuration',
;; and `#import'.

;;; Code:

(require 'glsl-mode)

(defgroup glslfx nil
  "Major mode for GLSLFX shader files."
  :group 'languages)

(defcustom glslfx-enable-outline-minor-mode nil
  "When non-nil, enable `outline-minor-mode' in `glslfx-mode'."
  :type 'boolean
  :group 'glslfx)

(defvar glslfx-section-header-regexp
  (rx bol "--" (+ blank)
      (group (or "glsl" "layout" "configuration"))
      (? (+ blank) (group (+ (or word "." "_" "-"))))
      (* blank) eol)
  "Regexp matching GLSLFX section headers.")

(defvar glslfx-font-lock-keywords
  `(;; Highlight the GLSLFX file header, e.g. `-- glslfx version 0.1'.
    ;; The whole header is treated like a preprocessor directive, while the
    ;; version number itself is highlighted as a constant.
    (,(rx bol "--" (+ blank) "glslfx" (+ blank) "version" (+ blank)
          (group (+ (or digit "."))))
     (0 font-lock-preprocessor-face)
     (1 font-lock-constant-face nil t))

    ;; Highlight GLSLFX section headers, e.g. `-- glsl Mesh.Vertex',
    ;; `-- layout Mesh.Vertex', and `-- configuration'.  The section kind is
    ;; keyword-like, while the optional section name is function-name-like so it
    ;; stands out in navigation-heavy shader files.
    (,glslfx-section-header-regexp
     (1 font-lock-keyword-face)
     (2 font-lock-function-name-face nil t))

    ;; Highlight long GLSLFX separator/comment lines, commonly written as
    ;; `--- ------------------------------------------------------------------'.
    (,(rx bol "---" (* nonl))
     . font-lock-comment-face)

    ;; Highlight GLSLFX imports, e.g. `#import $TOOLS/hdSt/shaders/foo.glslfx'.
    ;; The directive itself is preprocessor-like, and the imported path is
    ;; string-like.
    (,(rx bol "#import" (+ blank) (group (+ nonl)))
     (0 font-lock-preprocessor-face)
     (1 font-lock-string-face nil t)))
  "Additional font-lock keywords for `glslfx-mode'.")

(defvar glslfx-imenu-generic-expression
  `(("GLSL" ,(rx bol "--" (+ blank) "glsl" (+ blank)
                 (group (+ (or word "." "_" "-")))) 1)
    ("Layout" ,(rx bol "--" (+ blank) "layout" (+ blank)
                   (group (+ (or word "." "_" "-")))) 1)
    ("Configuration" ,(rx bol "--" (+ blank) "configuration") 0))
  "Imenu expressions for GLSLFX section headers.")

(defvar glslfx-outline-regexp
  (rx bol "--" (+ blank) (or "glsl" "layout" "configuration"))
  "Regexp matching GLSLFX outline headings.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.glslfx\\'" . glslfx-mode))

;;;###autoload
(define-derived-mode glslfx-mode glsl-mode "GLSLFX"
  "Major mode for editing GLSLFX shader files.

This mode derives from `glsl-mode' and adds highlighting/navigation for
GLSLFX container sections."
  (font-lock-add-keywords nil glslfx-font-lock-keywords)
  (setq-local imenu-generic-expression glslfx-imenu-generic-expression)
  (setq-local outline-regexp glslfx-outline-regexp)
  (setq-local outline-heading-end-regexp "\n")
  ;; Keep GLSL comment behavior because most GLSLFX editing happens in GLSL
  ;; chunks.  Lines beginning with `--' are section markers or metadata, not
  ;; the primary comment syntax for code editing.
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-padding "")
  (when glslfx-enable-outline-minor-mode
    (outline-minor-mode 1)))

(provide 'glslfx-mode)

;;; glslfx-mode.el ends here
