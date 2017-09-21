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

;; define bundler compilation mode
(define-compilation-mode bundler-compilation-mode "Bundler compilation"
  "Compilation mode for Bundler output."
  (add-hook 'compilation-filter-hook 'restaurant/colorize-compilation-buffer nil t))

(defun bundle-command (cmd)
  "Run CMD in an async buffer."
  (let ((default-directory (bundle-locate-gemfile)))
    (compile cmd 'bundler-compilation-mode)))

(defun bundle-install-restaurant-gems ()
  (interactive)
  (when restaurant/enable-rvm
    (rvm-use-default))
  (bundle-gemfile (locate-source-file "Gemfile"))
  (bundle-install))

(defhooklet restaurant/bundle enh-ruby-mode t
  (local-set-key (kbd "<S-f3>") 'bundle-update))

;;; restaurant-bundler.el ends here
