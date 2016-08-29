;;; restaurant-help.el --- help and versioning  -*- lexical-binding: t -*-

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

(defun restaurant-version (&optional short)
  (interactive)
  (let ((version "0.1.CURRENT")) ;; autoconf-anchor
    (if short
	(princ version)
      (princ (concat "Restaurant Chef IDE version: " version ".\nBuild with Emacs version:\n" (emacs-version) "\n")))))

(define-colored-compilation-mode selfdeps-installation-mode "Installing Restaurant required dependencies")

(defun bundle-install-selfdeps (&optional buffer result &rest rest)
  (interactive)
  (setq
   compilation-finish-functions
   (remove 'bundle-install-selfdeps compilation-finish-functions)) ; ``delete'' is not working properly sometimes
  (if (or (null result) (string-match "^finished" result))
      (progn
	(cd restaurant/source-directory)
	(compile
	 (concat
	  ". "
	  (expand-file-name "~/.rvm/scripts/rvm") ;; init required env. variables before start
	  "; bundle install ")
	 'selfdeps-installation-mode))
    (error "RVM installation failed. Can not continue.")))

(defun startup-wizard ()
  (interactive)
  (add-to-list 'compilation-finish-functions
	       'bundle-install-selfdeps)
  (call-interactively 'rvm-install-rvm))

;;; restaurant-help.el ends here
