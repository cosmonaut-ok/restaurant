;;; lib.el --- TODO:  -*- lexical-binding: t -*-

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
;;; restaurant speciall functions
;;;
(require 'cl-lib)

(defun restaurant/customize ()
  (interactive)
  (customize-group 'restaurant))

;; reload configuration
(defun reinit ()
  "Reload all restaurant configuration"
  (interactive)
  (let ((restaurant/do-bootstrap nil))
    (load-file user-init-file)))

(defun get-terminal-emulator ()
  (let ((terms '("konsole" "mate-terminal" "gnome-terminal" "terminator" "rxvt" "xterm")))
    (cl-labels ((get-first-existing-term (termlist)
					 (cond ((not termlist) nil)
					       ((call-process "which" nil nil nil (car termlist))
						(replace-regexp-in-string "\n$" "" (shell-command-to-string (concat "which" " " (car termlist)))))
					       (t (get-first-existing-term (cdr termlist))))))
	       (get-first-existing-term terms))))

(defun open-console ()
  (interactive)
  (if restaurant/use-external-terminal-emulator
      (call-process restaurant/terminal-emulator)
    (term "/bin/bash")))

;;;; Common Functions
(defun add-auto-mode (mode &rest files)
  "Connect file type to some"
  (dolist (f files)
    (add-to-list 'auto-mode-alist (cons f mode))))

(defun flash-cross ()
  (interactive)
  (require 'hl-line)
  (require 'col-highlight)
  (col-highlight-flash)
  (hl-line-flash))

;;;
;;; common compilation options
;;;

(defun get-first-n-list-elements (n list)
  (cond ((null list) nil)
	 ((= n 0) nil)
	 (t (cons (car list) (get-first-n-list-elements (- n 1) (cdr list))))))

(defvar exit-status 0)
;; Idea from <https://gist.github.com/jwiegley/fd78e747f27a90cb67a2>.
(defun notify-compilation-result (buffer result)
  "Notify about the ended compilation in BUFFER.
  This function is intended to be used in
  `compilation-finish-functions'."
  (with-current-buffer buffer
    (unless (eq major-mode 'grep-mode)
      (let ((urgency))
	(if (= exit-status 0)
	    (setq urgency 'normal)
	  (setq urgency 'critical))
	(when restaurant/notify-on-build
	  (notify "Build"
		  (format (concat "Buffer: %s\n"
				  "Command: %s\n"
				  "Result: %s")
			  ;;;
			  (buffer-name buffer)
			  ;;;
			  (concat (get-first-n-list-elements
				   20 (string-to-list compile-command)) "...")
			  ;;;
			  result
			  )
		  :urgency urgency
		  :icon "restaurant"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro verbose-message (&rest message)
  "Print MESSAGE only in verbose mode."
  `(when restaurant/enable-verbose (message ,@message)))

(defmacro defhooklet (name mode condition &rest body)
  "This is the test NAME, MODE, CONDITION, BODY."
  `(add-hook ',(intern (concat (symbol-name mode) "-hook"))
	     #'(lambda ()
			(when ,condition
			  (verbose-message "Launching ``%s'' hooklet on ``%s'' mode" ',name ',mode)
			  ,@body))))

(defun restaurant/enable-verbose-messages ()
  (interactive)
  (custom-set-variables
   '(restaurant/enable-verbose t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-sublist-by-fist-element (name map)
  (cond ((null map) nil)
	((eq name (caar map))
	 (car map))
	(t (find-tool-bar-item-by-name name (cdr map)))))

(cl-defun tool-bar-delete-item (name &optional (map tool-bar-map))
  (let ((item (find-sublist-by-fist-element name (cdr map))))
    (delete item map)))

(defmacro tool-bar-add-item-for-mode (icon def key mode &rest rest)
  `(tool-bar-add-item ,icon ,def ,key :active-modes '(,mode) ,@rest))

;;; restaurant-lib.el ends here
