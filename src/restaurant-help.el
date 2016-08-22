;;; version.el --- TODO:  -*- lexical-binding: t -*-

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

(defun bundle-install-selfdeps ()
  (interactive)
  (cd restaurant/source-directory)
  (compile
   (concat
    "source "
    (expand-file-name "~/.rvm/scripts/rvm")
    "; bundle install ")))


(defun startup-wizard ()
  (interactive)
  (call-interactively 'rvm-install-rvm)
  (call-interactively 'bundle-install-selfdeps))
;;; restaurant-version.el ends here
