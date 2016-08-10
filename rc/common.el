;;; package --- Summary restaurant-common

;;; Commentary:

;;; Code:

(require 'user-directories)
(require 'hideshowvis)
(require 'redo)

(add-to-list 'load-path (locate-source-file "data"))

(defun add-auto-mode (mode &rest files)
  "Connect file type to some"
  (dolist (f files)
    (add-to-list 'auto-mode-alist (cons f mode))))

;;;
;;; customization groups
;;;

(defgroup restaurant nil
  "Customization for ``Restaurant`` IDE, based on Emacs."
  :group 'emacs)

(defgroup restaurant/face nil
  "Color schemas, fonts etc"
  :group 'restaurant
  )

;;;
;;; prog-mode tweaks
;;;
(defcustom restaurant/max-line-length 135
  "Defines maximum recommended line length in program"
  :type 'integer
  :group 'restaurant/face
  )

(defvar restaurant-max-line-length (concat "^[^\n]\\{"
                                         (number-to-string restaurant/max-line-length)
                                         "\\}\\(.*\\)$"))

(defcustom restaurant/indent-level 2
  "Defines maximum recommended line length in program"
  :type 'integer
  :group 'restaurant/face
  )

(defun restaurant/highlight-prog-mode-strings ()
  ;; highlight FIXME/TODO/BUG keywords
  (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t)))
  (font-lock-add-keywords nil '(("\\<\\(DONE\\):" 1 font-lock-doc-face t)))
  ;; highlight too long lines
  ;; (font-lock-add-keywords nil '((restaurant-max-line-length 1 font-lock-warning-face t))) ; FIXME:
  )
(add-hook 'prog-mode-hook #'restaurant/highlight-prog-mode-strings)

;;;
;;; Make prog mode more usable
;;;
(defcustom restaurant/show-paren-mode t
  "Defines maximum recommended line length in program"
  :type 'boolean
  :group 'restaurant/face
  )

(defcustom restaurant/highlight-parentheses-mode t
  "Defines maximum recommended line length in program"
  :type 'boolean
  :group 'restaurant/face
  )

(defcustom restaurant/linum-mode t
  "Defines maximum recommended line length in program"
  :type 'boolean
  :group 'restaurant/face
  )

(defcustom restaurant/require-final-newline t
  "Defines maximum recommended line length in program"
  :type 'boolean
  :group 'restaurant/face
  )

(defcustom restaurant/highlight-current-line t
  "Highlight current line"
  :type 'boolean
  :group 'restaurant/face
  )

(defcustom restaurant/highlight-current-column t
  "Highlight current column"
  :type 'boolean
  :group 'restaurant/face
  )

(defcustom restaurant/activate-code-browser t
  "Automatically activate code browser, when program file opens"
  :type 'boolean
  :group 'restaurant/face
  )

(defcustom restaurant/why-so-serious t
  "Lets enable some fun"
  :type 'boolean
  :group 'restaurant/face
  )

(defun flash-cross ()
  (interactive)
  (col-highlight-flash)
  (hl-line-flash))

(defun restaurant/customize-prog-mode ()
  (show-paren-mode restaurant/show-paren-mode)
  ;; number lines
  (linum-mode restaurant/linum-mode)
  ;; highlight current column
  (when restaurant/highlight-current-column
    (require 'col-highlight)
    (column-highlight-mode restaurant/highlight-current-column))
  ;; highlight current line
  (hl-line-mode restaurant/highlight-current-line)
  ;; why so serious?
  (when restaurant/why-so-serious
    (require 'nyan-mode)
    (nyan-mode t))
  ;; fill column indicator
  (require 'fill-column-indicator)
  ;; 
  (fci-mode 1)
  (setq fci-rule-column restaurant/max-line-length)
  ;; (line-move-visual 1)
  (yas-minor-mode-on)
  (hs-minor-mode 1)
  (hideshowvis-minor-mode 1)
  (hs-hide-initial-comment-block)
  (company-mode 1)
  (company-quickhelp-mode 1)
  (hideshowvis-minor-mode 1)
  (auto-fill-mode -1)
  ;; projectile
  (require 'projectile)
  (projectile-mode 1)

  (subword-mode 1)
  (when restaurant/highlight-parentheses-mode
    (require 'highlight-parentheses)
    (highlight-parentheses-mode restaurant/highlight-parentheses-mode))
  ;; highlight words
  (font-lock-mode 1)

  ;; 'parenthesis of 'expression highlight just parens/highlight entire expression
  (setq show-paren-style 'parenthesis)

  ;; (setq indent-tabs-mode t) ;; Turn Off Tab Character
  (setq standard-indent restaurant/indent-level) ;; standard indent to 2 rather that 4
  (setq require-final-newline restaurant/require-final-newline)
  
  ;; company
  (local-set-key (kbd "<backtab>") 'company-complete)
  
  (local-set-key (kbd "S-SPC") 'hs-toggle-hiding)
  
  (local-set-key (kbd "C-c C-f") 'flash-cross)
  
  (local-set-key (kbd "RET") 'newline-and-indent)

  ;; (imenu-tree) ;; Open left imenu automatically TODO: does not working
  ;; comment/uncomment region
  ;; (local-set-key "\C-c:" 'uncomment-region)
  ;; (local-set-key "\C-c;" 'comment-region)
  ;; (local-set-key "\C-c\C-c" 'comment-region)
  ;; / comment/uncomment region
  ;; (sr-speedbar-open)
  (when restaurant/activate-code-browser
    (ecb-activate))
  ;; dtrt
  ;; (require 'dtrt-indent)
  ;; (dtrt-indent-mode t)
  ;; (dtrt-indent-max-merge-deviation 1)
  (message "Prog mode enabled. USE Shift+SPACE to show or hide blocks"))

(add-hook 'prog-mode-hook #'restaurant/customize-prog-mode)

;;;
;;; Text-mode functions and hooks
;;;
(defun restaurant/init-text-mode ()
  (show-paren-mode -1)
  (linum-mode -1)
  (hs-minor-mode -1)
  (auto-fill-mode 1)
  (subword-mode -1)
  (highlight-parentheses-mode -1)
  ;; (font-lock-mode -1)
  (hl-line-mode -1)
  (auto-fill-mode 1)
  ;; (sr-speedbar-close)
  ;; (ecb-deactivate)
  (message "Text mode enabled"))

(add-hook 'text-mode-hook 'restaurant/init-text-mode)
;;;; / Text-mode functions and hooks

;;;
;;; some tweaks
;;;
;; Remove visual line from buffer
(add-hook 'minibuffer-setup-hook (lambda ()
                                   (visual-line-mode -1)))
;; WAT
(setq stack-trace-on-error nil)

;; flycheck
(defun restaurant/flycheck-setup ()
  (require 'flycheck)
  (flycheck-mode 1)
  (flycheck-pos-tip-mode 1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(defun flycheck-print-current-checker (args)
  "Print checker for current buffer.
ARGS is dummy"
  (interactive "P")
  (print (flycheck-get-checker-for-buffer)))

(add-hook 'prog-mode-hook #'restaurant/flycheck-setup)

;; company mode tweaks
(require 'company)
(require 'company-quickhelp)
(add-hook 'after-init-hook 'global-company-mode)

(setq company-idle-delay 0
      company-auto-complete t
      company-minimum-prefix-length 3
      company-dabbrev-downcase nil
      company-dabbrev-ignore-case nil
      company-dabbrev-code-ignore-case nil)

(define-key company-active-map (kbd "C-n") #'company-select-next)
(define-key company-active-map (kbd "C-p") #'company-select-previous)

(eval-after-load 'company
  (lambda ()
    (setq company-quickhelp-delay 1)
    (define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup restaurant/backup nil
  "Miscelation parameters"
  :group 'restaurant
  )

(defgroup restaurant/misc nil
  "Miscelation parameters"
  :group 'restaurant
  )

(defgroup restaurant/system nil
  "System parameters for restaurant"
  :group 'restaurant
  )

(defcustom restaurant/autobackup t
  "Automatically activate code browser, when program file opens"
  :type 'boolean
  :group 'restaurant/backup
  )

(defcustom restaurant/backup-directory t
  "Automatically activate code browser, when program file opens"
  :type 'boolean
  :group 'restaurant/backup
  )

(defcustom restaurant/familiar-copy-paste-cut t
  "Bind familiar Control+c, Control+x and Control+v, instead emacs default keybindings"
  :type 'boolean
  :group 'restaurant/system
  )

(when restaurant/familiar-copy-paste-cut
  ;; https://www.emacswiki.org/emacs/CuaMode
  (cua-mode t))

(custom-set-variables
 '(yes-or-no-p 'y-or-n-p) ;replace y-e-s by y
 '(inhibit-startup-message t)           ;no splash screen
 ;; backup
 '(make-backup-files restaurant/autobackup)
 '(use-backup-dir restaurant/autobackup) ;use backup directory
 '(backup-directory-alist '((".*" . (locate-user-config-file "backups")))) ;; TODO: FIX
 '(version-control t) ;; Enable versioning with default values (keep five last versions, I think!)
 ;; dired/highlight
 '(delete-old-versions t)
 '(query-replace-highlight t)           ;highlight during query
 '(search-highlight t)                  ;highlight incremental search
 '(ls-lisp-dirs-first t)                ;display dirs first in dired
 '(initial-frame-alist (quote ((fullscreen . maximized)))) ; start maximized
 )

;; Recently edited files in menu
(recentf-mode 1)

;; Dired customizations
(setq dired-listing-switches "-Al")

;; Word completion customizations
;; (setq dabbrev-always-check-other-buffers t)
;; (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")

;; Column & line numbers in mode bar
(column-number-mode t)
(line-number-mode t)

(defcustom restaurant/tabbar t
  "Automatically activate code browser, when program file opens"
  :type 'boolean
  :group 'restaurant/face
  )

;; Tabbar mode
(defun tabbar-init ()
  (tabbar-mode 1)
  (setq tabbar-use-images nil)
  (global-set-key [S-left] 'tabbar-backward-tab)
  (global-set-key [S-right] 'tabbar-forward-tab))

(when restaurant/tabbar (tabbar-init))

;; Set visible bell
(setq visible-bell t)

;; set frame (window in window-managers terms) size
(when window-system (set-frame-size (selected-frame) 190 96))

;; Set time format to display
(load-library "time")
(setq display-time-24hr-format t
      display-time-mail-file t
      display-time-form-list (list 'time 'load)
      display-time-day-and-date t)
(display-time)

;; ido
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)

;; If we read a compressed file, uncompress it on the fly:
;; (this works with .tar.gz and .tgz file as well)
(setq auto-compression-mode 1)

;; Auto Fill mode
(setq-default fill-column restaurant/max-line-length) ;; the fill column number
(global-visual-line-mode t) ;; wrap long lines visually to several lines

;; Uniquify buffers
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)

;;;
;;; restaurant speciall commands
;;;

(defun restaurant/customize ()
  (interactive)
  (customize-group 'restaurant))

;; new menu items
(define-key global-map [menu-bar restaurant]
  (cons "Restaurant" (make-sparse-keymap "Restaurant")))

(define-key global-map [menu-bar restaurant customize-restaurant]
  (cons "Customize Restaurant" (make-sparse-keymap "Customize Restaurant")))

(define-key global-map
  [menu-bar restaurant customize-restaurant restaurant/customize]
  '("Restaurant Customization Menu" . restaurant/customize))

(define-key global-map
  [menu-bar restaurant customize-restaurant customize-themes]
  '("Customize Themes" . customize-themes))

(define-key global-map
  [menu-bar restaurant customize-restaurant customize]
  '("Global Customization Menu" . customize))

(define-key global-map
  [menu-bar restaurant hide-all-blocks]
  '("Hide all code blocks" . hs-hide-all))

(define-key global-map
  [menu-bar restaurant show-all-blocks]
  '("Show all code blocks" . hs-show-all))

;; reload configuration
(defun reinit ()
  "Reload all restaurant configuration"
  (interactive)
  (let ((restaurant/do-bootstrap nil))
    (load-file user-init-file)))

(define-key global-map
  [menu-bar restaurant reinit]
  '("Reload Restaurant configuration" . reinit))

(defun get-terminal-emulator ()
  (let ((terms '("konsole" "mate-terminal" "gnome-terminal" "terminator" "rxvt" "xterm")))
    (cl-labels ((get-first-existing-term (termlist)
					 (cond ((not termlist) nil)
					       ((shell-command (concat "which" " " (car termlist)))
						(replace-regexp-in-string "\n$" "" (shell-command-to-string (concat "which" " " (car termlist)))))
					       (t (get-first-existing-term (cdr termlist))))))
	       (get-first-existing-term terms))))

;; add button open terminal here
(defcustom restaurant/default-terminal-emulator (get-terminal-emulator)
  "Default terminal emulator"
  :type 'string
  :group 'restaurant/system)

(defun open-console (&optional command)
  (interactive)
  (if command
      (async-shell-command (concat restaurant/default-terminal-emulator " " "-e" " " command))
    (async-shell-command restaurant/default-terminal-emulator)))

;;;
;;; initialize package
;;;
(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("org" . "http://orgmode.org/elpa/")))

(package-initialize)

(provide 'restaurant-common)

;;; common.el ends here
