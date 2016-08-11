(defvar restaurant/source-directory (file-name-directory load-file-name))
(defvar restaurant/list-load-components '("common" "company" "ruby" "chef" "ecb" "markdown" "web" "yasnippet" "popup-menu" "menubar" "toolbar" "theme"))

(load (concat restaurant/source-directory "rc/user-directories.el"))
;;;; DIRS:
;; user-restaurant-config-directory
;; user-restaurant-data-directory
;; user-restaurant-cache-directory
;; user-restaurant-runtime-directory
;; user-restaurant-lisp-directory
;; user-restaurant-documents-directory

(defvar restaurant/packages-installed-p (locate-source-file ".packages.installed.p"))

(princ (locate-source-file ".packages.installed.p"))

(load (locate-source-file "bootstrap.el"))
(require 'el-get)
(el-get)

;; set init and custom file
(mkdir restaurant/user-config-directory t)

(setq user-init-file (locate-source-file "init.el")
      user-emacs-directory restaurant/user-config-directory
      custom-file (locate-user-config-file "restaurant.custom.el")
      local-file (locate-user-config-file "restaurant.el"))

;; create custom file if it does not exists
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))

;; load rc files
;; (load (concat restaurant/source-directory "load.el"))
(dolist (file restaurant/list-load-components)
  (load (locate-source-file (concat "rc" "/" file ".el"))))

(when (file-exists-p local-file)
  (load local-file))

;; load custom file
(when (file-exists-p custom-file)
  (load custom-file))

(message "Welltome to the Restaurant.")
;;; init.el ends here
