;; ci-init.el --- This file is used as an init script in the CI -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Xichen(Sichem) Zhou

;; Author: Xichen(Sichem) Zhou ("xzhou@xeechou.net")

;;; Commentary:

;;; Code:

(message "Running Emacs in CI mode.")

(let* ((scripts-dir (file-name-directory (or load-file-name buffer-file-name)))
       (root-dir (expand-file-name "../../../" scripts-dir)))
  (message "Calculated root directory is \"%s\"" root-dir)

  (message "Loading \"init.el\"")
  (load (expand-file-name "init.el" root-dir)))
