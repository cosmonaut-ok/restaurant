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
(require 'url)
;; RVM installation hack
(defvar rvm-installation-url "https://get.rvm.io/")

(defvar rvm-installation-key "409B6B1796C275462A1703113804BB82D39DC0E3")

(defun rvm-colorize-compilation-buffer ()
  "Colorize RVM compile buffer output."
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))

(define-compilation-mode rvm-installation-mode "RVM installation"
  "Installation mode for RVM."
  (add-hook 'compilation-filter-hook 'rvm-colorize-compilation-buffer nil t))

(defun rvm-install-rvm (ruby)
  (interactive "sWhich ruby do you want to install?: ")
  (let ((installator (locate-user-cache-file ".rvm.sh")))
    (progn (gpg-install-key rvm-installation-key)
	   (download-file rvm-installation-url installator)
	   (compile (concat
		     "/bin/bash "
		     installator
		     " stable"
		     " --ruby="
		     ruby
		     " --auto-dotfiles"
		     (when restaurant/use-bundler
		       " --gems=bundler"))))))

(defun rvm-install-ruby (ruby)
  (interactive "sWhich ruby do you want to install?: ")
  (compile
   (concat
    "source "
    (expand-file-name "~/.rvm/scripts/rvm")
    "; rvm install "
    ruby)))

;;; restaurant-rvm.el ends here
