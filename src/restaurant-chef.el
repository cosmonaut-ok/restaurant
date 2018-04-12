;;; restaurant-chef.el --- make Restaurant bootstrap and early boot  -*- lexical-binding: t -*-

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

;; TODO

;;; Code:

(defvar restaurant/chef-keywords-list
  '("action"
    "apt_repository"
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
    "options"
    "override"
    "owner"
    "package"
    "path"
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
    "sensitive"
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

(require 'chef-mode)

(require 'flycheck) ;; TODO: it's hack. Need to fix it
(let ((chef-file-full-path (concat
			    (file-name-as-directory restaurant/chefdk-home)
			    (file-name-as-directory "bin")
			    "chef")))
  (eval-after-load "flycheck"
    `(when (and restaurant/enable-flycheck restaurant/enable-chef restaurant/enable-foodcritic)
       (flycheck-define-checker chef-foodcritic
				"A Chef cookbooks syntax checker using Foodcritic.
See URL `http://acrmp.github.io/foodcritic/'."
				:command ,(if restaurant/enable-chefdk
					      `(,chef-file-full-path "exec" "foodcritic" (option-list "--tags" flycheck-foodcritic-tags) source)
					    '("foodcritic" (option-list "--tags" flycheck-foodcritic-tags) source)
					    ;;("foodcritic" (option-list "--tags" flycheck-foodcritic-tags) source)
				)
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
				     (locate-dominating-file parent-dir "resources")
				     (locate-dominating-file parent-dir "providers")
				     ;; Knife Solo
				     ;; http://matschaffer.github.io/knife-solo/#label-Init+command
				     (locate-dominating-file parent-dir "cookbooks"))))
				:next-checkers ((warnings-only . ruby-rubocop))))))

;; yas-chef-mode
(defhooklet restaurant/chef-add-extra-snippets chef-mode t
  (yas-activate-extra-mode 'chef-mode))

(defhooklet restaurant/imenu-replace chef-mode t
  (setq imenu-generic-expression
        '(("Ruby structures" "^\\( *\\(def\\|class\\|module\\) +.+\\)" 1)
          ("Actions and Resources" "^\\( *\\([a-zA-Z][a-zA-Z0-9]\\).+ +.+do\\)" 1)
          ("Included Receipes" "^\\( *\\(include_recipe\\) +.+\\)" 1)
          ))
  (setq imenu-create-index-function 'imenu-default-create-index-function)
  )

(defhooklet restaurant/chef-mode-init enh-ruby-mode restaurant/enable-chef
  (dolist (res restaurant/chef-keywords-list)
    ;; (pushnew res enh-ruby-extra-keywords :test 'string-equal)
    (add-to-list 'enh-ruby-extra-keywords res)
    ;; (add-to-list 'enh-ruby-defun-beg-keywords res)
    )
  ;; add "action" to method-list
  ;; (add-to-list 'enh-ruby-defun-beg-keywords "action")

  (erm-reset) ;; `erm-reset'  will need to be called in order for any global changes to take effect.
  (chef-mode 1)
  )

(defun restaurant-chefdk-switch ()
  (custom-set-variables
   '(berkshelf-chefdk-home-directory restaurant/chefdk-home)
   '(test-kitchen-chefdk-home-directory restaurant/chefdk-home)
   '(rspec-chefdk-home-directory restaurant/chefdk-home))
  ;;
  (if restaurant/enable-chefdk
      (custom-set-variables
       '(berkshelf-use-chefdk-when-possible t)
       '(rspec-use-chefdk-when-possible t)
       '(test-kitchen-use-chefdk-when-possible t)
       ;; '(chef-knife-command (concat (restaurant-chefdk-command "chef") " exec knife"))
       ;;
       ;; '(knife-kitchen-use-chefdk-when-possible t)
       ;; '(foodcritic-use-chefdk-when-possible t)
       ;; '(bundler-use-chefdk-when-possible t)
       ;; '(gem-use-chefdk-when-possible t)
       ;; '(rubocop-use-chefdk-when-possible t)
       ;; '(bundler-use-chefdk-when-possible t)
       ;;
       )
    (custom-set-variables
     '(berkshelf-use-chefdk-when-possible nil)
     '(rspec-use-chefdk-when-possible nil)
     '(test-kitchen-use-chefdk-when-possible nil)
     ;; '(chef-knife-command "knife")
     ;;
     ;; '(knife-kitchen-use-chefdk-when-possible t)
     ;; '(foodcritic-use-chefdk-when-possible t)
     ;; '(bundler-use-chefdk-when-possible t)
     ;; '(gem-use-chefdk-when-possible t)
     ;; '(rubocop-use-chefdk-when-possible t)
     ;; '(bundler-use-chefdk-when-possible t)
     ;;
     )))

(defun restaurant-bundler-switch ()
  (if restaurant/enable-bundler
      (progn
	(custom-set-variables
	 '(berkshelf-use-bundler-when-possible t)
	 '(rspec-use-bundler-when-possible t)
	 '(test-kitchen-use-bundler-when-possible t)
	 ;;
	 ;; '(knife-kitchen-use-bundler-when-possible t)
	 ;; '(foodcritic-use-bundler-when-possible t)
	 ;; '(bundler-use-bundler-when-possible t)
	 ;; '(gem-use-bundler-when-possible t)
	 ;; '(rubocop-use-bundler-when-possible t)
	 ;;
	 )
	;; (if (not restaurant/enable-chefdk)
	;;     (custom-set-variables
	;;      '(chef-use-bundler t)))
        )
    (progn
      (custom-set-variables
       '(berkshelf-use-bundler-when-possible nil)
       '(rspec-use-bundler-when-possible nil)
       '(test-kitchen-use-bundler-when-possible nil)
       ;;
       ;; '(knife-kitchen-use-bundler-when-possible nil)
       ;; '(foodcritic-use-bundler-when-possible nil)
       ;; '(bundler-use-bundler-when-possible nil)
       ;; '(gem-use-bundler-when-possible nil)
       ;; '(rubocop-use-bundler-when-possible nil)
       ;;
       ;; '(chef-use-bundler nil)
       ))))

(defhooklet restaurant/chefdk-switcher prog-mode t
  (restaurant-chefdk-switch))

(defhooklet restaurant/bundler-switcher prog-mode t
  (restaurant-bundler-switch))
;;; restaurant-chef.el ends here
