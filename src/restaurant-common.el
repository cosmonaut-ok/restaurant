;;; restaurant-common.el --- TODO:  -*- lexical-binding: t -*-

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

(require 'cl-lib)
(require 'user-directories)
(require 'redo+)
(require 'f)
(require 'notify)
(require 'ansi-color)
;; (require 'github-notifier)
(require 'popup)
(require 'ssh)

;;;
;;; Global config
;;;

(defun restaurant/colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))

;;;; Global default configuration parameters

(custom-set-variables
 '(yes-or-no-p 'y-or-n-p) ;replace y-e-s by y
 '(inhibit-startup-message t) ;no splash screen
 ;; backup
 '(make-backup-files restaurant/autobackup)
 '(use-backup-dir restaurant/autobackup) ;use backup directory
 '(version-control t) ;; Enable versioning with default values (keep five last versions, I think!)
 '(delete-old-versions t)
 '(query-replace-highlight t)           ;highlight during query
 '(search-highlight t)                  ;highlight incremental search
 '(ls-lisp-dirs-first t)                ;display dirs first in dired
 '(initial-frame-alist (quote ((fullscreen . maximized)))) ; start maximized
 )

;;;; visual mode
(global-visual-line-mode t) ;; wrap long lines visually to several lines
;; Remove visual line from buffer
(add-hook 'minibuffer-setup-hook '(lambda ()
                                   (visual-line-mode -1)))

;;;; enable cua mode
(defhooklet restaurant/cua-mode emacs-startup restaurant/familiar-copy-paste-cut
  ;; https://www.emacswiki.org/emacs/CuaMode
  (cua-mode t))

(defhooklet restaurant/cua-mode test-mode restaurant/familiar-copy-paste-cut
  (cua-mode t))

(defhooklet restaurant/cua-mode prog-mode restaurant/familiar-copy-paste-cut
  (cua-mode t))

;;;; Enable Drag stuff
(drag-stuff-global-mode 1)

;;;; Sync linux and eamcs clipboards
;; after copy Ctrl+c in Linux X11, you can paste by `yank' in emacs
(setq x-select-enable-clipboard t)

;; after mouse selection in X11, you can paste by `yank' in emacs
(setq x-select-enable-primary t)

;;;; Move lines/regions up/down
(require 'drag-stuff)
(drag-stuff-global-mode 1)
;; Drag line
;; To drag a line up and down. Put the cursor on that line and press <M-up> and <M-down>.
;; Drag lines
;; To drag several lines up and down. Select the lines you want to drag and press <M-up> and <M-down>.
;; Drag region
;; A region can be dragged to the left and right. Select the region you want to drag and press <M-left> and <M-right>.
;; Drag word
;; To drag a word. Place the cursor on the word and press <M-left> and <M-right>.
;; For more information, see comments in drag-stuff.el.

;;;; Recently edited files in menu
(recentf-mode 1)

;;;; Dired customizations
(setq dired-listing-switches "-Al")

;;;; Word completion customizations
(setq dabbrev-always-check-other-buffers t)
;; (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")

;; move semanticDB, srecode and ede to cache
(mkdir restaurant/user-cache-directory t)
(custom-set-variables
 '(ede-project-placeholder-cache-file (locate-user-cache-file "restaurant-ede-projects.el"))
 '(semanticdb-default-save-directory (locate-user-cache-file "restaurant-semanticdb"))
 '(srecode-map-save-file (locate-user-cache-file "restaurant-srecode-map.el")))

;;;; Column & line numbers in mode bar
(column-number-mode t)
(line-number-mode t)

;;;; Tabbar mode
(when restaurant/enable-tabbar
  (tabbar-mode 1)
  (setq tabbar-use-images nil)
  (global-set-key [S-left] 'tabbar-backward-tab)
  (global-set-key [S-right] 'tabbar-forward-tab))


;;;; Set visible bell
(setq visible-bell t)

;; ;; set frame (window in window-managers terms) size
;; (when window-system (set-frame-size (selected-frame) 190 96))

;;;; Set time format to display
(load-library "time")
(setq display-time-24hr-format t
      display-time-mail-file t
      display-time-form-list (list 'time 'load)
      display-time-day-and-date t)
(display-time)

;;;; ido
;; https://github.com/magnars/.emacs.d/blob/master/settings/setup-ido.el
(require 'ido)
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-show-count t)
(ido-better-flex/enable)
(setq ido-enable-flex-matching t)
(setq ido-vertical-define-keys 'C-n-C-p-up-and-down)

(global-set-key "\M-x" (lambda ()
			 (interactive)
			 (call-interactively (intern (ido-completing-read
						      "M-x " (all-completions "" obarray 'commandp))))))


;; dired mode too
(add-hook 'dired-mode-hook
	  '(lambda ()
	     (set (make-local-variable 'ido-enable-replace-completing-read) nil)))

;;;; If we read a compressed file, uncompress it on the fly:
;;;; (this works with .tar.gz and .tgz file as well)
(setq auto-compression-mode 1)

;;;; fill-column
(defun restaurant/fill-column ()
  (when restaurant/fill-column
    (setq-default fill-column restaurant/max-line-length)))

(add-hook 'prog-mode-hook 'restaurant/fill-column)

;;;; Uniquify buffers
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;; ;; WAT
;; (setq stack-trace-on-error nil)

;;;
;;; Prog mode configuration
;;;

(defvar restaurant-max-line-length (concat "^[^\n]\\{"
					   (number-to-string restaurant/max-line-length)
					   "\\}\\(.*\\)$"))

;;;
;;; activate generic options
;;;
(require 'magit)
(require 'magit-gh-pulls)

(defhooklet restaurant/generic-prog-mode-improvements prog-mode t
  ;; highlight FIXME/TODO/BUG keywords
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))
  (font-lock-add-keywords nil '(("\\<\\(DONE\\):" 1 font-lock-doc-face t)))
  ;; auto-fill mode
  (auto-fill-mode -1)
  ;; projectile
  (require 'projectile)
  ;; magit
  (setq magit-completing-read-function 'magit-ido-completing-read)
  ;;
  (subword-mode 1)
  (font-lock-mode 1)
  ;; Drag and move selcted
  (drag-stuff-mode 1)

  (local-set-key (kbd "C-c C-f") 'flash-cross)
  (local-set-key (kbd "RET") 'newline-and-indent)

  (message "Prog mode enabled. USE Shift+SPACE to show or hide blocks"))

;; Remove trailing whitespaces
(defhooklet restaurant/delete-trailing-whitespaces before-save t
  (delete-trailing-whitespace 1))

;;
(when (and restaurant/check-parens-before-save buffer-file-name)
  (add-hook 'before-save-hook
	    'check-parens
	    nil t))

;; magit-gh-pulls
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
;;;
;;; indicate paren
;;;
(defhooklet restaurant/parenth-management prog-mode restaurant/indicate-parentheses
  (require 'paren)
  (require 'highlight-parentheses)
  ;; show pait to current parenth
  (show-paren-mode 1)
  (setq show-paren-delay 0)
  ;; indicate other parentheses
  (highlight-parentheses-mode 1)
  ;; 'parenthesis of 'expression highlight just parens/highlight entire expression
  (setq show-paren-style 'parenthesis))

;;;
;;; linum
;;;
(defhooklet restaurant/linum prog-mode restaurant/linum-mode
    (require 'linum)
    (linum-mode 1))

;;;
;;; highlight-current-column
;;;
(defhooklet restaurant/highlight-current-column prog-mode restaurant/highlight-current-column
  (require 'col-highlight)
  (column-highlight-mode 1))

;;;
;;; highlight-current-line
;;;
(defhooklet restaurant/hl-line prog-mode restaurant/highlight-current-line
    (require 'hl-line+)
    (hl-line-mode 1))

;;;
;;; nyan
;;;
(defhooklet restaurant/nyan prog-mode restaurant/why-so-serious
    (nyan-mode 1))

;;;
;;; fill-column-indicator
;;;
(defvar restaurant-last-max-line-length restaurant/max-line-length)

(defhooklet restaurant/fill-column-indicator prog-mode restaurant/fill-column
  (require 'fill-column-indicator)
  (fci-mode 1)
  (setq fci-rule-column restaurant/max-line-length)
  ;; highlight too long lines
  (when restaurant/highlight-too-long-lines
    (let ((restaurant-max-line-length (concat "^[^\n]\\{"
					      (number-to-string restaurant/max-line-length)
					      "\\}\\(.*\\)$"))
	  (restaurant-previous-max-line-length (concat "^[^\n]\\{"
						       (number-to-string restaurant-last-max-line-length)
						       "\\}\\(.*\\)$")))
      (font-lock-remove-keywords nil (list (list (concat "^[^\n]\\{" restaurant-previous-max-line-length "\\}\\(.*\\)$") 1 font-lock-warning-face t)))
      (font-lock-add-keywords nil (list (list restaurant-max-line-length 1 font-lock-warning-face t)))))
  (setq restaurant-max-line-length restaurant/max-line-length))

;;;
;;; hideshow
;;;
(defhooklet restaurant/hideshow prog-mode restaurant/enable-hide-show-blocks
  (require 'hideshow)
  (require 'hideshowvis)
  (hs-minor-mode 1)
  (hideshowvis-enable)
  (hideshowvis-symbols)
  (hs-hide-initial-comment-block)
  (local-set-key (kbd "S-SPC") 'hs-toggle-hiding))

;;;
;;; ispell
;;;
;; (defhooklet restaurant/ispell prog-mode restaurant/enable-spell-checking
;;   (flyspell-prog-mode)) ;; Check strings for spelling errors

;;;
;;; projectile
;;;
(defhooklet restaurant/projectile prog-mode t
  (require 'projectile)
  (projectile-mode 1))

;;;
;;; require-final-newline
;;;
(defhooklet restaurant/require-final-newline prog-mode restaurant/require-final-newline
  (setq require-final-newline 1))

;;;
;;; indent-level
;;;
(defhooklet restaurant/indent-level prog-mode t
  (custom-set-variables
   '(standard-indent restaurant/indent-level)))

;;;
;;; ssh
;;;
(defhooklet restaurant/ssh ssh-mode t
  (setq ssh-directory-tracking-mode t)
  (shell-dirtrack-mode t)
  (setq dirtrackp nil))

;;;
;;; common compilation options
;;;

;;;; Notify result from compilation buffer
(add-to-list 'compilation-finish-functions
	     'notify-compilation-result)

;;;
;;; helm-swoop
;;;
(require 'helm-swoop)
(global-set-key (kbd "C-M-S") 'helm-swoop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; set key to open-console
(global-set-key (kbd "<f12>") 'open-console)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; initialize package
;;;
(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("org" . "http://orgmode.org/elpa/")))

(package-initialize)

;;; restaurant-common.el ends here
