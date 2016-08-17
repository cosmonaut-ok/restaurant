;;; theme.el --- TODO:  -*- lexical-binding: t -*-

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

(require 'color-theme)
(color-theme-initialize)
;; (color-theme-restaurant)

;; ;; add it to theme
;; (setq hl-paren-colors
;;       (quote
;;        ("orange1" "gray" "greenyellow" "green1" "springgreen1" "cyan1" "slateblue1" "magenta1" "purple")))

(setq custom-theme-directory (concat restaurant/source-directory "data/themes"))

(mkdir restaurant/user-data-directory t)
(add-to-list 'custom-theme-load-path (concat restaurant/user-data-directory "/themes")) ;; TODO: add all themes in this folder

(load-theme 'restaurant t)

;; (setq restaurant-theme-force-faces-for-mode t)

;; (set-foreground-color "gray")
