(defvar restaurant/chef-keywords-list
  '("apt_repository"
    "ark"
    "at_exit" ;; ?
    "baseurl"
    "bash"
    "before"
    "chef_gem"
    "ChefSpec"
    "command"
    "context"
    "cookbook_file"
    "cron"
    "default"
    "define"
    "depends"
    "describe"
    "directory"
    "describe"
    "docker_container"
    "docker_image"
    "execute"
    "expect"
    "file"
    "fog_key_pair" ;; ?
    "gem_package"
    "git"
    "group"
    "hostsfile_entry"
    "htpasswd"
    "node"
    "include_recipe"
    "iptables_rule"
    "it"
    "jenkins_command"
    "jenkins_plugin"
    "jenkins_script"
    "link"
    "log"
    "logrotate_app"
    "mode"
    "mysql_client"
    "mysql_config"
    "mysql_database"
    "mysql_database_user"
    "mysql_service"
    "not_if"
    "notifies"
    "should"
    "subscribes"
    "ohai"
    "only_if"
    "override"
    "owner"
    "package"
    "php_fpm_pool"
    "python_pip"
    "python_service"
    "rackspace_dns_record"
    "rbenv_execute"
    "rbenv_gem"
    "rbenv_ruby"
    "remote_file"
    "rpm_package"
    "rsync_serve"
    "ruby_block"
    "service"
    "simple_iptables_rule"
    "source"
    "stub_command"
    "sudo"
    "swap_file"
    "sysctl_param"
    "tar_extract"
    "template"
    "user"
    "users_manage"
    "user_ulimit"
    "variables"
    "yum_package"
    "yum_repository"
    ))

(defun restaurant/chef-mode-init ()
  (when restaurant/enable-chef
    (require 'chef-mode)
    (chef-mode 1)
    ;;
    (dolist (res restaurant/chef-keywords-list)
      (pushnew res enh-ruby-extra-keywords :test 'string-equal))
    ))

(add-hook 'enh-ruby-mode-hook 'restaurant/chef-mode-init)

(require 'flycheck) ;; TODO: it's hack. Need to fix it
(eval-after-load "flycheck"
  '(when (and restaurant/enable-flycheck restaurant/enable-chef restaurant/enable-foodcritic)
     (flycheck-define-checker chef-foodcritic
			      "A Chef cookbooks syntax checker using Foodcritic.                                                                                             
See URL `http://acrmp.github.io/foodcritic/'."
			      :command ("foodcritic" (option-list "--tags" flycheck-foodcritic-tags) source)
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
			      :next-checkers ((warnings-only . ruby-rubocop)))))

     
   ;;   (flycheck-define-checker chef-foodcritic-metadata
   ;; 			      "A Chef cookbooks syntax checker using Foodcritic.

   ;; URL `http://acrmp.github.io/foodcritic/'."
   ;; 			      ;; Use `source-inplace' to allow resource discovery with relative paths.
   ;; 			      ;; foodcritic interprets these as relative to the source file, so we need to
   ;; 			      ;; stay within the source tree.  See
   ;; 			      ;; https://github.com/flycheck/flycheck/pull/556
   ;; 			      :command ("foodcritic"
   ;; 					(option-list "--tags" flycheck-foodcritic-tags)
   ;; 					source)
   ;; 			      :error-patterns
   ;; 			      ((error line-start (message) ": " (file-name) ":" line line-end))
   ;; 			      :modes (enh-ruby-mode ruby-mode)
   ;; 			      :predicate
   ;; 			      (lambda ()
   ;; 				(let ((parent-dir (file-name-directory
   ;; 						   (directory-file-name
   ;; 						    (expand-file-name default-directory)))))
   ;; 				  (and (or
   ;; 					;; Chef CookBook
   ;; 					;; http://docs.opscode.com/chef/knife.html#id38
   ;; 					(locate-dominating-file parent-dir "recipes")
   ;; 					(locate-dominating-file parent-dir "providers")
   ;; 					(locate-dominating-file parent-dir "resources")
   ;; 					;; Knife Solo
   ;; 					;; http://matschaffer.github.io/knife-solo/#label-Init+command
   ;; 					(locate-dominating-file parent-dir "cookbooks"))
   ;; 				       (string= (file-name-nondirectory (buffer-file-name)) "metadata.rb"))))
   ;; 			      :next-checkers ((warnings-only . ruby-rubocop)))))

;;;
;;; foodcritic
;;;
(defun restaurant/foodcritic-init ()
  (when restaurant/enable-foodcritic
    (require 'foodcritic)
    (foodcritic-mode 1)
    (auto-revert-mode 1) ;; TODO: is it needed here?
    ))

(add-hook 'enh-ruby-mode-hook 'restaurant/foodcritic-init)

;;; chef.el ends here
