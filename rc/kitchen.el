;;;
;;; test-kitchen
;;;
(defun restaurant/chef-kitchen-init ()
  (require 'test-kitchen))

(add-hook 'chef-mode-hook 'restaurant/chef-kitchen-init)

;; patch for original test-kitchen.el. It does not supports automatic login
(defcustom test-kitchen-login-command "kitchen login"
  "The command used for converge project.")

(defun test-kitchen-login ()
  (interactive)
  (let ((root-dir (test-kitchen-locate-root-dir)))
    (if root-dir
        (let ((default-directory root-dir))
          (term test-kitchen-login-command))
      (error "Couldn't locate .kitchen.yml!"))))
;;
