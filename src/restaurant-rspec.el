;;; rspec.el --- rspec support  -*- lexical-binding: t -*-

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

(require 'rspec-mode)

;;;
;;; rspec
;;;
(custom-set-variables
 '(compilation-scroll-output t)
 '(rspec-command-options ""))

;;;; add snippets after rspec loaded
(eval-after-load 'rspec-mode
 '(rspec-install-snippets))

(defhooklet restaurant/rspec enh-ruby-mode t
  (local-set-key (kbd "<f8>") 'rspec-verify-all)
  (local-set-key (kbd "<C-f8>") 'rspec-verify)
  (local-set-key (kbd "<S-f8>") 'rspec-verify-single)
  )

;;; restaurant-rspec.el ends here
