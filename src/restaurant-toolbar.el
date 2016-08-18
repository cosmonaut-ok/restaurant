;;; toolbar.el --- TODO:  -*- lexical-binding: t -*-

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

;; add
(define-key-after (default-value 'tool-bar-map) [separator-10] menu-bar-separator)
;;
(tool-bar-add-item "terminal" 'open-console 'open-terminal-here)
;;
(define-key-after (default-value 'tool-bar-map) [separator-11] menu-bar-separator)
;;
(tool-bar-add-item "hsplit" 'split-window-below 'split-window-horizontally)
(tool-bar-add-item "vsplit" 'split-window-right 'split-window-vertically)
(tool-bar-add-item "minimize" 'delete-window 'remove-split-and-hide-current-window)
(tool-bar-add-item "one" 'delete-other-windows 'leave-only-current-window)
;;
(define-key-after (default-value 'tool-bar-map) [separator-12] menu-bar-separator)
