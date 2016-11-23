;;; berkshelf.el --- make Restaurant bootstrap and early boot  -*- lexical-binding: t -*-

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

(require 'berkshelf)

(defvar restaurant/use-berkshelf t) ;; TODO: convert to defcustom

;; small berkshelf hack ;-)
(defun berkshelf-colorize-compilation-buffer ()
  "Colorize berkshelf compile buffer output."
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))

;; define test kitchen compilation mode
(define-compilation-mode berkshelf-compilation-mode "Berkshelf compilation"
  "Compilation mode for Berkshelf output."
  (add-hook 'compilation-filter-hook 'berkshelf-colorize-compilation-buffer nil t))

(defun berks-command (cmd)
  "Run CMD in an async buffer."
  (let ((default-directory (berks-locate-berksfile)))
    (compile cmd 'berkshelf-compilation-mode)))

;;; restaurant-berkshelf.el ends here
