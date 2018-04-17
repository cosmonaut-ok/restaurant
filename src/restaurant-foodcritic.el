;;; foodcritic.el --- chef foodcritic support  -*- lexical-binding: t -*-

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

(require 'foodcritic)

(defhooklet restaurant/foodcritic-init enh-ruby-mode restaurant/enable-foodcritic
  (foodcritic-mode 1)
  (auto-revert-mode 1) ;; TODO: is it needed here?
  ;; set foodctiric checking command
  (let ((chef-file-full-path (concat
            (file-name-as-directory restaurant/chefdk-home)
            (file-name-as-directory "bin")
            "chef")))
    (setq foodcritic-check-command
    (if restaurant/enable-chefdk
        (concat chef-file-full-path " exec " "foodcritic"
          (if foodcritic-ignore-tags " -t " ""))
      (concat "foodcritic"
        (if foodcritic-ignore-tags " -t " "")))))
  ;; keybindings
  (local-set-key (kbd "<f5>") 'foodcritic-check-project)
  (local-set-key (kbd "<S-f5>") 'foodcritic-check-current-file)
  )

;;; restaurant-foodcritic.el ends here
