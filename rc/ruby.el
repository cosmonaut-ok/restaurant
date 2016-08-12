;;; Package --- Restaurant

;;; Commentary:

;;; Code:

(require 'ruby-mode)
(require 'ruby-electric)
(require 'ruby-hash-syntax)

(add-auto-mode 'ruby-mode
               "Rakefile\\'" "\\.rake\\'" "\\.rxml\\'"
               "\\.rjs\\'" "\\.irbrc\\'" "\\.pryrc\\'" "\\.builder\\'" "\\.ru\\'"
               "\\.gemspec\\'" "Gemfile\\'")

(setf ruby-indent-level restaurant/indent-level)

;;;
;;; robe mode: code navigtion, documentation
;;;
(add-hook 'ruby-mode-hook 'robe-mode)

(defgroup restaurant/ruby nil
  "Ruby-specific options"
  :group 'restaurant
)

(defcustom restaurant/robe-auto-enable nil
  "Enable ROBE automatically, when open ruby file"
  :type 'boolean
  :group 'restaurant/ruby
  )

(defun restaurant/ruby-robe ()
  (when restaurant/robe-auto-enable
    (require 'robe)
    (push 'company-robe company-backends)
    (setq robe-turn-on-eldoc nil)
    (robe-start)))

;; push robe to company backends
(defun restaurant/push-company-robe ()
  (push 'company-robe company-backends))

(eval-after-load "company"
  '(add-hook 'robe-mode-hook #'restaurant/push-company-robe))

(add-hook 'ruby-mode-hook 'restaurant/ruby-robe)

;;;
;;; ruby-electric
;;;
(add-hook 'ruby-mode-hook #'ruby-electric-mode)

;;;
;;; rvm
;;;
(defun restaurant/ruby-rvm ()
  (rvm-use-default)
  (require 'rvm)
  ;; connect rvm+robe
  (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
    (rvm-activate-corresponding-ruby))
  )

(add-hook 'ruby-mode-hook 'restaurant/ruby-rvm)

;;;
;;; rubocop
;;;
(defun restaurant/ruby-rubocop ()
  (require 'rubocop))

(add-hook 'ruby-mode-hook #'rubocop-mode)
(add-hook 'ruby-mode-hook #'auto-revert-mode)

(add-hook 'ruby-mode-hook (lambda ()
                            (setf ruby-indent-level restaurant/indent-level)))

;;;
;;; flycheck/flymake
;;;
(require 'flycheck)
(add-hook 'ruby-mode-hook #'flycheck-mode)
(setq-default flycheck-check-syntax-automatically '(save mode-enabled))

;; flymake
(require 'flymake)
;; I don't like the default colors :)
(set-face-background 'flymake-errline "red4")
(set-face-background 'flymake-warnline "dark slate blue")

(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

;;;
;;; rspec
;;;
(add-to-list 'auto-mode-alist '("/spec/" . ruby-mode))
(add-to-list 'auto-mode-alist '("/spec/" . rspec-mode))

(provide 'restaurant-ruby)

;;; ruby.el ends here
