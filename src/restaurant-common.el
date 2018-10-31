;;; restaurant-common.el --- common restaurant features  -*- lexical-binding: t -*-

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
;; (require 'popup)
(require 'ssh)

;;;
;;; Global config
;;;

;;;; Global default configuration parameters

(custom-set-variables
 '(yes-or-no-p 'y-or-n-p)   ;replace y-e-s by y
 '(inhibit-startup-message t)   ;no splash screen
 ;; backup
 '(make-backup-files restaurant/autobackup)
 '(use-backup-dir restaurant/autobackup) ;use backup directory
 '(version-control t) ;; Enable versioning with default values (keep five last versions, I think!)
 '(backup-by-copying t)
 '(delete-old-versions t)
 '(backup-directory-alist
   `((".*" . ,restaurant/backup-directory))) ; don't litter my fs tree
 '(auto-save-file-name-transforms
   `((".*" ,restaurant/backup-directory t)))
 `(auto-save-interval ,restaurant/autobackup-interval)
 ;; other
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

(defhooklet restaurant/cua-mode text-mode restaurant/familiar-copy-paste-cut
  (cua-mode t))

(defhooklet restaurant/cua-mode prog-mode restaurant/familiar-copy-paste-cut
  (cua-mode t))

(custom-set-variables
;;;; Sync linux and eamcs clipboards
 ;; after copy Ctrl+c in Linux X11, you can paste by `yank' in emacs
 '(x-select-enable-clipboard t)

 ;; after mouse selection in X11, you can paste by `yank' in emacs
 '(x-select-enable-primary t)
 )

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

;; load cedet components
(load "cedet")
(load "semantic")
(load "srecode")
(load "eieio")
(load "ede")

;; move semanticDB, srecode and ede to cache
(mkdir restaurant/user-cache-directory t)
(custom-set-variables
 '(ede-project-placeholder-cache-file (locate-user-cache-file "restaurant-ede-projects.el"))
 '(semanticdb-default-save-directory (locate-user-cache-file "restaurant-semanticdb"))
 '(srecode-map-save-file (locate-user-cache-file "restaurant-srecode-map.el")))

;; setup semantic/imenu autorefresh
(custom-set-variables
 '(imenu-auto-rescan t)
 )

;; semantic global mode
(global-semantic-idle-completions-mode t)
(global-semantic-decoration-mode t)
(global-semantic-highlight-func-mode t)
(global-semantic-show-unmatched-syntax-mode t)

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
;; (require 'ido)
;; (require 'ido-vertical-mode)
;; (require 'ido-completing-read+)
;; ;;
;; (ido-mode 1)
;; (ido-vertical-mode 1)
;; (ido-better-flex/enable)

;; (custom-set-variables
;;  '(ido-vertical-show-count t)
;;  '(ido-enable-flex-matching t)
;;  '(ido-vertical-define-keys 'C-n-C-p-up-and-down)
;;  )

;; (global-set-key "\M-x" (lambda ()
;;        (interactive)
;;        (call-interactively (intern (ido-completing-read
;;                   "M-x " (all-completions "" obarray 'commandp))))))

;; set ido-completing-read as default completing function
;; (setq ido-cr+-replace-completely t)
;; (setq-default completing-read-function 'ido-completing-read)

;;;; set key to enable whitespace mode
(global-set-key (kbd "C-c w") 'whitespace-mode)
(global-set-key (kbd "C-c C-w") 'whitespace-mode)

;;;; If we read a compressed file, uncompress it on the fly:
;;;; (this works with .tar.gz and .tgz file as well)
(custom-set-variables
 '(auto-compression-mode 1))

;;;; fill-column
(defun restaurant/fill-column ()
  (when restaurant/fill-column
    (setq-default fill-column restaurant/max-line-length)))

(add-hook 'prog-mode-hook 'restaurant/fill-column)

;;;; Uniquify buffers
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

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
  ;; (setq magit-completing-read-function 'magit-ido-completing-read)
  ;;
  (subword-mode 1)
  (font-lock-mode 1)
  ;; Drag and move selcted
  (drag-stuff-mode 1)
  ;; set global indent-tabs-mode
  (setq indent-tabs-mode restaurant/indent-tabs-mode)
  ;; highlight-symbol-mode
  (highlight-symbol-mode restaurant/highlight-symbol)

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
(require 'projectile)
(projectile-global-mode 1)

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
;;; Ediff customizations
;;;
(setq ediff-ignore-similar-regions t)
(setq ediff-use-last-dir t)
(setq ediff-diff-options " -b ")

;;;
;;; common compilation options
;;;

;;;; Notify result from compilation buffer
(add-to-list 'compilation-finish-functions
       'notify-compilation-result)

;;;
;;; helm-swoop
;;;
;; (require 'helm-swoop)
;; (global-set-key (kbd "C-M-S") 'helm-swoop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; set some generic keys
(global-set-key (kbd "<f12>") 'open-console)
(global-set-key (kbd "<S-f12>") 'ielm)

(global-set-key (kbd "<f7>") 'isearch-forward)
(global-set-key (kbd "<S-f7>") 'highlight-regexp)
(global-set-key (kbd "<C-S-f7>") 'unhighlight-regexp)
(global-set-key (kbd "<M-f7>") 'projectile-grep)
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

;;;
;;; automatically remove old backups
;;;
(when restaurant/clear-autobackups
  (message "Deleting old backup files...")
  (let ((week (* 60 60 24 7))
  (current (float-time (current-time))))
    (dolist (file (directory-files temporary-file-directory t))
      (when (and (backup-file-name-p file)
     (> (- current (float-time (fifth (file-attributes file))))
        week))
  (message "%s" file)
  (delete-file file)))))

;; set scratch message
(setq initial-scratch-message ";; Welcome to Restaurant's CUTTING BOARD\n;; Feel free to use it, like your scratchpad\n;; and perform temporary text editings here\n")

(defhooklet restaurant/rename-scratch-buffer after-change-major-mode t
  ;; Rename scratch buffer
  (if (and (get-buffer "*scratch*") (not (get-buffer "scratch")))
      (with-current-buffer "*scratch*"
  (rename-buffer "*cutting-board*")
  ;;
  (insert (propertize initial-scratch-message)))))


;;;; helm
(require 'helm-config)
(require 'helm-grep)
(require 'helm-lib)
(require 'helm-swoop)
(require 'helm-projectile)

(defhooklet restaurant/generic-helm-hooklet helm-minibuffer-set-up t
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
			      `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

;; global set keys
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
(define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
(define-key helm-grep-mode-map (kbd "p")  'helm-grep-mode-jump-other-window-backward)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c r") 'helm-recentf)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-c h o") 'helm-occur)

(global-set-key (kbd "C-c h w") 'helm-wikipedia-suggest)
(global-set-key (kbd "C-c h g") 'helm-google-suggest)

(global-set-key (kbd "C-c h x") 'helm-register)
;; (global-set-key (kbd "C-x r j") 'jump-to-register)

(define-key 'help-command (kbd "C-f") 'helm-apropos)
(define-key 'help-command (kbd "r") 'helm-info-emacs)
(define-key 'help-command (kbd "C-l") 'helm-locate-library)

;; helm swoop
(global-set-key (kbd "C-c h o") 'helm-swoop)
(global-set-key (kbd "C-c s") 'helm-multi-swoop-all)

;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)

;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)

;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)

;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows t)

;; Split direcion. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)

;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color t)

;; helm-projectile
(helm-projectile-on)
(setq projectile-completion-system 'helm)
(setq projectile-indexing-method 'alien)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(custom-set-variables
 '(helm-google-suggest-use-curl-p t)
 '(helm-scroll-amount 4) ; scroll 4 lines other window using M-<next>/M-<prior>
 ;; helm-quick-update t ; do not display invisible candidates
 '(helm-ff-search-library-in-sexp t) ; search for library in `require' and `declare-function' sexp.

 ;; you can customize helm-do-grep to execute ack-grep
 ;; helm-grep-default-command "ack-grep -Hn --smart-case --no-group --no-color %e %p %f"
 ;; helm-grep-default-recurse-command "ack-grep -H --smart-case --no-group --no-color %e %p %f"
 '(helm-split-window-in-side-p t) ;; open helm buffer inside current window, not occupy whole other window

 '(helm-echo-input-in-header-line t)

 ;; helm-candidate-number-limit 500 ; limit the number of displayed canidates
 '(helm-ff-file-name-history-use-recentf t)
 '(helm-move-to-line-cycle-in-source t) ; move to end or beginning of source when reaching top or bottom of source.
 '(helm-buffer-skip-remote-checking t)

 '(helm-mode-fuzzy-match t)

 '(helm-buffers-fuzzy-matching t) ; fuzzy matching buffer names when non-nil
					; useful in helm-mini that lists buffers
 '(helm-org-headings-fontify t)
 ;; helm-find-files-sort-directories t
 ;; ido-use-virtual-buffers t
 '(helm-semantic-fuzzy-match t)
 '(helm-M-x-fuzzy-match t)
 '(helm-imenu-fuzzy-match t)
 '(helm-lisp-fuzzy-completion t)
 ;; helm-apropos-fuzzy-match t
 '(helm-buffer-skip-remote-checking t)
 '(helm-locate-fuzzy-match t)
 '(helm-display-header-line nil)
 )

(add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)

;;; restaurant-common.el ends here
