;;; restaurant-rvm.el --- TODO:  -*- lexical-binding: t -*-

;; Copyright (C) 2016 Alexander aka 'CosmonauT' Vynnyk

;; Maintainer: cosmonaut.ok@zoho.com
;; Keywords: internal
;; Package: restaurant

;; This file is part of Restaurant.

;; Restaurant is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Restaurant is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Restaurant.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO:

;;; Code:

(require 'rvm)

;; RVM installation hack
(define-compilation-mode rvm-installation-mode "RVM installation"
  "Installation mode for RVM."
  (add-hook 'compilation-filter-hook 'bundler-colorize-compilation-buffer nil t))

(defun rvm-install-rvm ()
  (interactive)
  (let ((cmd "gpg --keyserver hkp://keys.gnupg.net --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3 && curl -sSL https://get.rvm.io | bash"))
    (compile cmd 'rvm-installation-mode)))




;;; loads URL in buffer and execute command there
(defun get-exec-url (&optional url download-dir download-name)
  (let ((download-buffer (url-retrieve-synchronously url)))
    (with-current-buffer download-buffer
      (save-excursion
	(set-buffer download-buffer)
	;; we may have to trim the http response
	(goto-char (point-min))
	(re-search-forward "^$" nil 'move)
	(forward-char)
	(delete-region (point-min) (point))
	;; execute shell command
	(shell-command-on-region (point-min) (point-max) "/bin/bash" t)))))






;;; restaurant-rvm.el ends here
