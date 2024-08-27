;;; per-device.el --- manage your per device settings in Emacs -*- lexical-binding: t
;;; -*-

;; Copyright (C) 2024 Xichen Zhou
;; Author: Xichen Zhou <sichem.zh@gmail.com>
;; Maintainer: Xichen Zhou <sichem.zh@gmail.com>

;; Package-Requires: ((emacs "28.1") (cl-lib "1.0) (ligature "1.0"))
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

;; This package provides convenience function to setup some per-device
;; settings. The package will search the following locations in order to find a
;; working path:
;; 1. $PERDEV_EL_CONFIG_PATH
;; 2. $XDG_CONFIG_HOME/.perdev.el
;; 3. ${HOME}/.perdev.el
;;; Code:

(require 'custom)

(defgroup perdev nil
  "Emacs per device settings"
  :group 'perdev)

(defun perdev-setting-location ()
  (let ((env-path (getenv "PERDEV_EL_CONFIG_PATH"))
	(xdg-path (expand-file-name ".perdev.el" (getenv
							"XDG_CONFIG_HOME")))
	(dot-path (expand-file-name ".perdev.el" (getenv "HOME"))))

    (cond ((and env-path (file-exists-p env-path)) env-path)
	  ((and xdg-path (file-exists-p xdg-path)) xdg-path)
	  ((and dot-path (file-exists-p dot-path)) dot-path)
	  (t nil))))

(defun perdev-load-personal-el ()
  ;;if file exists and has changed since last visit
  (when (and (perdev-setting-location)
	     (file-has-changed-p (perdev-setting-location)))
    (load-file (perdev-setting-location))))

;;;###autoload
(defun perdev-get-value (quoted-symbol default-value)
  "Getting a user-defined `quoted-symbol' value with `'default-value'."
  (perdev-load-personal-el)
  (if (boundp quoted-symbol)
      (symbol-value quoted-symbol)
    default-value))

(defun perdev-get-evaluated-value (quoted-func default-value &rest args)
  "Getting a value evaluated by a function `quoted-func' with `default-value'."
  (perdev-load-personal-el)
  (if (fboundp quoted-func)
      (apply quoted-func args)
    default-value))

(provide 'perdev)
