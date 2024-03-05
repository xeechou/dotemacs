;;; appr.el --- manage your look/feel for Emacs -*- lexical-binding: t
;;; -*-

;; Copyright (C) 2024 Xichen Zhou
;; Author: Xichen Zhou <sichem.zh@gmail.com>
;; Maintainer: Xichen Zhou <sichem.zh@gmail.com>

;; Package-Requires: ((emacs "28.1") (cl-lib "1.0) (ligature "1.0"))
;; URL: https://github.com/xeechou/appearance.el
;; Version: 0.0.1

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides convenience function to setup the font faces for Emacs

;;; Code:

(require 'ligature)
(require 'frame)
(require 'custom)
(require 'appr-ligature)

(defgroup appr nil
  "Emacs font and theme setting"
  :group 'appr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; font
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom appr-default-font-size 13
  "Default font size."
  :type 'number
  :group 'appr)

(defun appr--setup-font-loop (func font-list)
  "Inner loop to setup a font using FONT-LIST."
  (catch 'result ;;catch block returns 'result or nil
    (dolist (font font-list)
      (when (funcall func font)
	(throw 'result t)))))

(defun appr-setup-fontset-font (range font-list)
  "Set a font for a given RANGE (eg: 'han or '(#x1f300 . #x1fad0) if
inside FONT-LIST."
  (appr--setup-font-loop
   #'(lambda (font)
       (when (member font (font-family-list))
	 (progn
	   ;; (message "%s is available" font)
	   (set-fontset-font t range font)
	   t)))
   ;;the list it goes through
   font-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ligature
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom appr-ligature-supported-modes '(prog-mode text-mode outline-mode)
  "The list of ligature supported mode."
  ;;type copied from linum mode
  :type '(repeat (sexp :tag "Major mode")))

(defcustom appr-ligature-symbols-alist `(,appr-ligature-fira
					 ,appr-ligature-iosevka
					 ,appr-ligature-cascadia
					 ,appr-ligature-jetbrain
					 ,appr-ligature-berkley
					 ,appr-ligature-monolisa)
  "The list of ligature symbols for the given font."
  ;;you can use push to add to alist, need to unquote. Check
  ;;http://xahlee.info/emacs/emacs/elisp_association_list.html
  :type `(alist
	  :key-type string
	  :value-type cons))

(defun appr--setup-ligature-font-inner (font-symbols)
  "Inner loop to setup the ligature fonts"
  (let* ((font    (car font-symbols))
	 (symbols (cdr font-symbols))
	 (size    (number-to-string appr-default-font-size))
	 (sized-font (concat font "-" size)))
    (if (member font (font-family-list))
	(progn
	  ;;TODO: should I use set-frame-font or custom-theme-set-faces?
	  (set-frame-font sized-font t t)
	  (ligature-set-ligatures appr-ligature-supported-modes symbols)
	  ;;return t
	  t)
      nil)))

;;tested
(defun appr-setup-ligature-font ()
  "setup the ligature font and symbol if available"
  (appr--setup-font-loop #'appr--setup-ligature-font-inner
			 appr-ligature-symbols-alist))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cjk font
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom appr-cjk-font-list '("WenQuanYi Micro Hei"
				"WenQuanYi Zen Hei"
				"Microsoft YaHei"
				"Microsoft JhengHei")
  "Supported CJK font"
  :group 'appr
  :type `(repeat (string :tag "CJK Font")))

;;TESTED
(defun appr-setup-cjk-font ()
  "Setup the CJK font, RETURN t if succeed nil otherwise."
  (appr-setup-fontset-font 'han appr-cjk-font-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emoji font
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom appr-emoji-font-list '("Noto Color Emoji" "Symbola" "Apple Color Emoji")
  "Supported CJK font"
  :group 'appr
  :type `(repeat (string :tag "Emoji Font")))

(defcustom appr-emoji-symbol-range (cons #x1f300 #x1fad0)
  "Emoji symbol range"
  :type 'cons
  :group 'appr)

;;TESTED
(defun appr-setup-emoji-font ()
  "Setup the Emoji font, RETURN t if succeed nil otherwise"
  (appr-setup-fontset-font appr-emoji-symbol-range appr-emoji-font-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; variable pitch font
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom appr-variable-pitch-font-list '("Fira Sans"
					   "Iosevka Aile"
					   "Iosevka Etoile")
  "Supported Variable Pitch font list."
  :group 'appr
  :type `(repeat (string :tag "Variable pitch font")))

(defun appr--setup-variable-pitch-font-inner (font)
  "Set variable-pitch FONT when available"
  (when (member font (font-family-list))
    (progn
      ;; (message "%s is available" font)
      (set-face-attribute
       'variable-pitch
       nil ;; frame name
       :family font)
      t)))

;;TESTED
(defun appr-setup-variable-pitch-font ()
  "Setup the Variable pitch font, RETURN t if succeed nil otherwise"
  (appr--setup-font-loop #'appr--setup-variable-pitch-font-inner
			 appr-variable-pitch-font-list))


(defun appr-setup ()
  (appr-setup-ligature-font)
  (appr-setup-cjk-font)
  (appr-setup-emoji-font)
  (appr-setup-variable-pitch-font))


;;;###autoload
(defun appr ()
  "Setup the appearances."
  (add-hook 'after-make-frame-functions
	    (lambda (frame)
	      "Configure faces on frame connection"
	      (when frame
		(select-frame frame))
	      (message "Setup up font for current frame")
	      (when (display-graphic-p)
		(appr-setup))))
  (add-hook 'window-setup-hook #'appr-setup))

(provide 'appr)
;;; appr.el ends here
