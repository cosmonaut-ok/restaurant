;;; restaurant-company.el --- enable company mode for autocompletions  -*- lexical-binding: t -*-

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
;;; company-mode
;;;
(require 'company)
(require 'company-quickhelp)

(custom-set-variables
 '(company-idle-delay 0.5)
 '(company-auto-complete t)
 '(company-minimum-prefix-length 3)
 '(company-dabbrev-downcase nil)
 '(company-dabbrev-ignore-case nil)
 '(company-dabbrev-code-ignore-case nil)
 '(company-tooltip-limit 20) ; bigger popup window
 '(company-tooltip-align-annotations 't) ; align annotations to the right tooltip border
 '(company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
 )

(define-key company-active-map (kbd "C-n") #'company-select-next)
(define-key company-active-map (kbd "C-p") #'company-select-previous)

(eval-after-load 'company
  (lambda ()
    (setq company-quickhelp-delay 1)
    (define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin)
    (global-set-key (kbd "<backtab>") 'company-complete)
    ))

(defun restaurant/local-push-company-backend (backend)
  "Add BACKEND to a buffer-local version of `company-backends'."
  (set (make-local-variable 'company-backends)
       (append (list backend) company-backends)))

;; (add-hook 'after-init-hook 'global-company-mode)
(defhooklet restaurant/company prog-mode t
  (company-mode 1))

;;;; TODO: it's experimental feature
(defun company-predictive (command &optional arg &rest ignored)
  "This is experimental predictive MODE for company-mode."
  (case command
    (prefix (let* ((text (downcase (word-at-point))))
              (set-text-properties 0 (length text) nil text)
              text))
    (candidates (predictive-complete arg))))

(restaurant/local-push-company-backend 'company-predictive)

;;
(restaurant/local-push-company-backend 'company-files)

;;; restaurant-company.el ends here
