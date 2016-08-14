(defun restaurant/chef-mode-init ()
  (when restaurant/enable-chef
    (require 'chef-mode)
    (chef-mode 1)
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

;;; chef.el ends here
