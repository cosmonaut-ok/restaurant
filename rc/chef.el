(require 'f)

(defun restaurant/chef-mode ()

  (require 'chef-mode)
  ;; (add-hook 'ruby-mode-hook
  ;;           #'(lambda ()
  ;;               (yas-activate-extra-mode 'chef-mode)))

  (flycheck-define-checker chef-foodcritic
    "A Chef cookbooks syntax checker using Foodcritic.
See URL `http://acrmp.github.io/foodcritic/'."
    :command ("foodcritic" source)
    :error-patterns
    ((error line-start (message) ": " (file-name) ":" line line-end))
    :modes (enh-ruby-mode ruby-mode)
    :predicate
    (lambda ()
      (let ((parent-dir (f-parent default-directory)))
        (or
         ;; Chef CookBook
         ;; http://docs.opscode.com/chef/knife.html#id38
         (locate-dominating-file parent-dir "recipes")
         ;; Knife Solo
         ;; http://matschaffer.github.io/knife-solo/#label-Init+command
         (locate-dominating-file parent-dir "cookbooks"))))
    :next-checkers ((warnings-only . ruby-rubocop)))
  )

(add-hook 'ruby-mode-hook #'restaurant/chef-mode)

;;;
;;; test-kitchen
;;;
(defun restaurant/chef-kitchen ()
  (require 'test-kitchen)
  (setq test-kitchen-destroy-command "kitchen destroy")
  (setq test-kitchen-list-command "kitchen list")
  (setq test-kitchen-test-command "kitchen test")
  (setq test-kitchen-converge-command "kitchen converge")
  (setq test-kitchen-verify-command "kitchen verify")
  )

(add-hook 'chef-mode-hook #'restaurant/chef-kitchen)

(defun test-kitchen-login ()
  (interactive)
  (let ((root-dir (test-kitchen-locate-root-dir)))
    (if root-dir
        (let ((default-directory root-dir)
              (out-buffer (get-buffer-create "*chef output*")))
          (open-console "kitchen login"))
      (error "Couldn't locate .kitchen.yml!"))))

;; kitchen customs
(defgroup restaurant/chef nil
  "Color schemas, fonts etc"
  :group 'restaurant
  )

(defcustom restaurant/test-kitchen-verbose-level "info"
  "Set test kitchen debug level"
  :type 'string
  :group 'restaurant/chef
  )

;;; chef.el ends here
