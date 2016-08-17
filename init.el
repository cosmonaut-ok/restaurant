;; check if ruby installed

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(when (not (call-process "which" nil nil nil "ruby"))
  (warn "WARNING! There is no ruby in system. Extended ruby/chef features are not supported"))

(defvar restaurant/source-directory (file-name-directory load-file-name))
(defvar restaurant/list-load-components '("lib" "face" "common" "company" "ruby" "rspec" "chef" "kitchen" "bundler" "foodcritic" "rvm" "codebrowser" "markdown" "yaml" "json" "web" "erb" "fly" "yasnippet" "popup-menu"  "menubar" "toolbar" "theme" "version"))

;; loading initial user-directories file
(let ((ud-file (concat restaurant/source-directory "src/user-directories.el")))
  (if noninteractive
      (progn (load ud-file) (byte-compile-file ud-file))
    (if (file-exists-p (concat ud-file "c")) ;; searching for elc files
	(load (concat ud-file "c"))
      (load ud-file))))

(defvar restaurant/packages-installed-p (locate-source-file "build"))

(load (locate-source-file "bootstrap.el"))
(require 'el-get)
(el-get)

;; set init and custom file
(mkdir restaurant/user-config-directory t)

(setq user-init-file (locate-source-file "init.el")
      user-emacs-directory restaurant/user-config-directory
      custom-file (locate-user-config-file "restaurant.el")
      local-file (locate-user-config-file "restaurant.el"))

;; create custom file if it does not exists
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

;; load rc files
(if noninteractive
    (progn
      ;; load el
      (dolist (file restaurant/list-load-components)
 	(load (locate-source-file (concat "src" "/" file ".el"))))
      ;; byte-compile to elc
      (dolist (file restaurant/list-load-components)
 	(byte-compile-file (locate-source-file (concat "src" "/" file ".el")))))
  ;;
  (dolist (file restaurant/list-load-components)
    (load (or (locate-source-file (concat "src" "/" file ".elc"))
	      (locate-source-file (concat "src" "/" file ".el"))))))
  
(when (file-exists-p local-file)
  (load local-file))

(message "Wellcome to the Restaurant. Please, choose your dishes from menu. Right click for Appetizer")
;;; init.el ends here
