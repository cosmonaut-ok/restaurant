;;; Package --- Restaurant

;;; Commentary:

;;; Code:

(require 'enh-ruby-mode)
(defalias 'ruby-mode 'enh-ruby-mode)

;; inf-ruby and robe
(require 'robe)
(require 'inf-ruby)
(inf-ruby)
(robe-start)

;;;
;;; Generic
;;;

;; Adapted from the method used by TextMate, this library provides a command
;; ruby-toggle-hash-syntax which attempts to automatically convert the
;; selected region of ruby code between 1.8 and 1.9 hash styles.
(require 'ruby-hash-syntax)

;; add auto-modes
(add-auto-mode 'enh-ruby-mode
               "Rakefile\\'" "\\.rake\\'" "\\.rxml\\'" "\\/spec\\/" "\\.rb\\'"
               "\\.rjs\\'" "\\.irbrc\\'" "\\.pryrc\\'" "\\.builder\\'" "\\.ru\\'"
               "\\.gemspec\\'" "Gemfile\\'")

;; TODO: add it to restaurant/init function
(setf ruby-indent-level restaurant/indent-level)

(eval-after-load 'ruby-mode
  '(define-key enh-ruby-mode-map (kbd "TAB") 'indent-for-tab-command))

;; set ruby indent level
(add-hook 'enh-ruby-mode-hook (lambda ()
				(setf ruby-indent-level restaurant/indent-level)))

;; set ruby indent tabs mode
(add-hook 'enh-ruby-mode-hook (lambda ()
				(setf ruby-indent-tabs-mode restaurant/indent-tabs-mode)))

;;;
;;; bundler
;;;
(require 'bundler)

;; small bundler hack ;-)
(defun bundler-colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))

;; define test kitchen compilation mode
(define-compilation-mode bundler-compilation-mode "Bundler compilation"
  "Compilation mode for Bundler output."
  (add-hook 'compilation-filter-hook 'bundler-colorize-compilation-buffer nil t))

(defun bundle-command (cmd)
  "Run cmd in an async buffer."
  (compile cmd 'bundler-compilation-mode))

;;;
;;; ruby-electric
;;;
(defun restaurant/ruby-electric-init ()
  (when restaurant/enable-electric
    (require 'ruby-electric)
    (ruby-electric-mode t)))

(add-hook 'enh-ruby-mode-hook #'restaurant/ruby-electric-init)

;;;
;;; ruby-tools
;;;
(defun restaurant/ruby-tools-init ()
  (when restaurant/enable-ruby-tools
    (require 'ruby-tools)
    (ruby-tools-mode 1)))

(add-hook 'enh-ruby-mode-hook 'restaurant/ruby-tools-init)

;;;
;;; ruby-refactor
;;;
(defun restaurant/ruby-refactor-init ()
  (when restaurant/enable-ruby-refactor
    (require 'ruby-refactor)
    (ruby-refactor-mode 1)))

(add-hook 'enh-ruby-mode-hook 'restaurant/ruby-refactor-init)

;;;
;;; rvm
;;;
(defun restaurant/rvm-init ()
  (when restaurant/enable-rvm
    ;; (rvm-use-default)
    (require 'rvm)
    ;; connect rvm+robe
    (when restaurant/enable-robe
      (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
	(rvm-activate-corresponding-ruby)))
    ))

(add-hook 'enh-ruby-mode-hook 'restaurant/rvm-init)

;;;
;;; robe mode: code navigtion, documentation
;;;
(defun restaurant/robe-init ()
  (when restaurant/enable-robe
    (setq robe-turn-on-eldoc t)
    (robe-mode 1)
    ;; integrate with company mode
    (push 'company-robe company-backends)))

(dolist (hook '(enh-ruby-mode-hook inf-ruby-mode-hook html-erb-mode-hook haml-mode))
  (add-hook hook
	    (lambda () (restaurant/local-push-company-backend 'company-robe))))

(add-hook 'enh-ruby-mode-hook 'restaurant/robe-init)

;;;
;;; rubocop
;;;
(defun restaurant/rubocop-init ()
  (when restaurant/enable-rubocop
    (require 'rubocop)
    (rubocop-mode 1)
    (auto-revert-mode 1) ;; TODO: is it needed here?
    ))

(add-hook 'enh-ruby-mode-hook 'restaurant/rubocop-init)

;;;
;;; flycheck
;;;
(defun restaurant/flycheck-ruby-init ()
  (when restaurant/enable-flycheck
    ;; (require 'flycheck) already activated in prog-mode
    (flycheck-mode 1)
    (setq-default flycheck-check-syntax-automatically '(save mode-enabled))))

(add-hook 'enh-ruby-mode-hook 'restaurant/flycheck-ruby-init)
;;;
;;; flymake
;;;
(defun restaurant/flymake-ruby-init ()
  (when restaurant/enable-flymake
    ;; (require 'flymake) already activated in prog-mode
    (require 'flymake-ruby)
    (flymake-ruby-load) ;; FIXME: not loading automatically
    (flymake-mode 1)))

(add-hook 'enh-ruby-mode-hook 'restaurant/flymake-ruby-init)

;;;
;;; ri
;;;
(defun restaurant/ri-yari-init ()
  (when restaurant/enable-ri
    (require 'yari)
    (defalias 'ri 'yari)
    (local-set-key [f1] 'yari)
    ))

(add-hook 'enh-ruby-mode-hook 'restaurant/ri-yari-init)

;;;
;;; hide/show blocks
;;;
;; When folding, take these delimiters into consideration
(add-to-list 'hs-special-modes-alist
	     '(ruby-mode
	       "\\(class\\|def\\|do\\|if\\|.each\\)" "\\(end\\)" "#"
	       (lambda (arg) (ruby-end-of-block)) nil))

;;;
;;; generic init
;;;
(defun restaurant/ruby-generic-init ()
  (inf-ruby-minor-mode 1)
  (when (executable-find "pry")
    (setq inf-ruby-default-implementation "pry"))
  )

(add-hook 'enh-ruby-mode-hook #'restaurant/ruby-generic-init)
;;; ruby.el ends here
