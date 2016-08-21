;;; bundler.el --- make Restaurant bootstrap and early boot  -*- lexical-binding: t -*-

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

;; TODO

;;; Code:

(require 'bundler)

(defvar restaurant/use-bundler t) ;; TODO: convert to defcustom

;; small bundler hack ;-)
(defun bundler-colorize-compilation-buffer ()
  "Colorize bundler compile buffer output."
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))

;; define test kitchen compilation mode
(define-compilation-mode bundler-compilation-mode "Bundler compilation"
  "Compilation mode for Bundler output."
  (add-hook 'compilation-filter-hook 'bundler-colorize-compilation-buffer nil t))

(defun bundle-command (cmd)
  "Run CMD in an async buffer."
  (compile cmd 'bundler-compilation-mode))

;;; restaurant-bundler.el ends here
