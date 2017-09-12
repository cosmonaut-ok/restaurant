;;; kitchen.el --- test kitchen support  -*- lexical-binding: t -*-

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

;;;
;;; test-kitchen
;;;

(defhooklet restaurant/chef-kitchen chef-mode
  (require 'test-kitchen)
  (local-set-key (kbd "<f9>") 'test-kitchen-converge)
  (local-set-key (kbd "<S-f9>") 'test-kitchen-verify)
  (local-set-key (kbd "<C-f9>") 'test-kitchen-converge-all)
  (local-set-key (kbd "<C-S-f9>") 'test-kitchen-verify-all)
  )

;; patch for original test-kitchen.el. It does not supports automatic login
(defcustom test-kitchen-login-command "kitchen login"
  "The command used for converge project.")

(defun test-kitchen-login (instance)
  (interactive (list (completing-read "Kitchen instance to login: " (split-string (test-kitchen-list-bare)))))
  (let ((root-dir (test-kitchen-locate-root-dir)))
    (if root-dir
        (let ((default-directory root-dir))
	  (with-current-buffer (term "/bin/bash")
	    (term-send-raw-string (concat test-kitchen-login-command " " instance "\n"))))
      (error "Couldn't locate .kitchen.yml!"))))

;; we use rvm and custom binary path to kitchen, so don't need 'chef exec'
(custom-set-variables
 '(test-kitchen-converge-command "kitchen converge")
 '(test-kitchen-destroy-command "kitchen destroy")
 '(test-kitchen-list-command "kitchen list")
 '(test-kitchen-test-command "kitchen test")
 '(test-kitchen-verify-command "kitchen verify")
 )
