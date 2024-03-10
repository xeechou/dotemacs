;;; appr-theme.el --- theme config for appr -*- lexical-binding: t
;;; -*-

;; Copyright (C) 2024 Xichen Zhou
;; Author: Xichen Zhou <sichem.zh@gmail.com>
;; Maintainer: Xichen Zhou <sichem.zh@gmail.com>

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

;;; Code:

(defgroup appr-theme nil
  "appr ligature symbols."
  :group 'appr
  :group 'theme)

(defcustom appr-light-theme-name nil
  "when not nil, defines the light theme you want to use"
  :type 'symbol)

(defcustom appr-light-theme-hour 8
  "starting hour of light-theme, in 24H format"
  :type 'integer
  :group 'appr-theme)

(defcustom appr-dark-theme-name nil
  "when not nil, defines the dark theme you want to use"
  :type 'symbol)

(defcustom appr-dark-theme-hour 17
  "starting hour of dark-theme, in 24H format"
  :type 'integer
  :group 'appr-theme)

(defvar appr-before-apply-theme-hook nil
  "Hooks to apply before theme change.")

(defvar appr-after-apply-theme-hook nil
  "Hooks to apply after theme change.")

(defun appr-theme-based-on-time ()
  "get the right theme based on the time of day"
  (let* ((ltime appr-light-theme-hour)
	 (dtime appr-dark-theme-hour)
	 (time  (current-time-string))
	 (clock (nth 3 (split-string time)))
	 (h-m-s (split-string clock ":"))
	 (hour  (string-to-number (car h-m-s))))
    (if (and (>= hour ltime) (<= hour dtime))
	appr-light-theme-name appr-dark-theme-name)))

;;;###autoload
(defun appr-apply-theme ()
  ;;apply theme based on time
  (let ((theme (appr-theme-based-on-time)))
    ;;only apply theme if not the current theme
    (unless (equal (list theme) custom-enabled-themes)
      ;;we disable current theme first
      (mapc #'disable-theme custom-enabled-themes)
      (progn (run-hooks 'appr-before-apply-theme-hook)
	     (load-theme theme t)
	     (run-hooks 'appr-after-apply-theme-hook)))))

(provide 'appr-theme)
