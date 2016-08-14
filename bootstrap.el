(defvar restaurant/source-directory (file-name-directory load-file-name))
(defvar restaurant/packages-installed-p (concat restaurant/source-directory "build"))
(defvar restaurant/elget-user-recipes-path (concat restaurant/source-directory "el-get-user/recipes"))
(defvar restaurant/do-bootstrap t)

(setq user-emacs-directory restaurant/source-directory)

(defvar *el-get-packages-list*
  '(
    ;; libraries and frameworks
    ;; "helm"
    "f"
    "s"
    "apel"
    ;; ruby
    "enh-ruby-mode"
    "ruby-electric"
    "ruby-hash-syntax"
    "robe-mode"
    ;; "helm-robe"
    "ruby-tools"
    "ruby-refactor"
    "rubocop"
    "rvm"
    "yari"
    "bundler"
    ;; "rspec-mode" ;; moved to scripts
    ;; completion
    "company-mode"
    "company-inf-ruby"
    "company-quickhelp"
    "company-web"
    ;; markdown
    "markdown-mode"
    "markdown-preview-mode"
    "markdown-toc"
    ;; yaml
    "yaml-mode"
    ;; json
    "json-mode"
    ;; xml
    ;; web
    "web-mode"
    "mmm-mode"
    ;; other
    "ecb"
    "sr-speedbar"
    "magit"
    "gh"
    "magit-gh-pulls"
    "color-theme"
    "fill-column-indicator"
    "highlight-parentheses"
    "col-highlight"
    "flycheck"
    "flycheck-pos-tip"
    "flymake-ruby"
    "tabbar"
    "yasnippet"
    "yasnippets"
    "yasnippet-snippets"
    "popup"
    "projectile"
    "ido-ubiquitous" ;; needed for magit
    ;; fun
    "nyan-mode"
    ;; TODO: make button instead search this file
    ;; "helm-swoop"
    ;; themes
    "color-theme-zenburn"
    "solarized-emacs"
    "birds-of-paradise-plus-theme"
    "color-theme-sanityinc"
    "dream-theme"
    "leuven-theme"
    ))

(defun init-elget-user-recipes ()
  (unless (file-directory-p restaurant/elget-user-recipes-path)
    (mkdir restaurant/elget-user-recipes-path t))
  (when (file-exists-p "/tmp/el-get-master.zip")
    (delete-file "/tmp/el-get-master.zip"))
  (when (file-directory-p "/tmp/el-get-master")
    (delete-directory "/tmp/el-get-master" t))
  (let ((download-buffer (url-retrieve-synchronously "https://github.com/dimitri/el-get/archive/master.zip")))
    (save-excursion
      (set-buffer download-buffer)
      ;; we may have to trim the http response
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move)
      (forward-char)
      (delete-region (point-min) (point))
      (write-file "/tmp/el-get-master.zip"))
    (shell-command "unzip -qq /tmp/el-get-master.zip -d /tmp/")
    (copy-directory "/tmp/el-get-master/recipes/" restaurant/elget-user-recipes-path nil t t)))

(defun init-el-get-packages ()
  "install el-get packages"
  (message "Installing el-get packages...")
  (el-get 'sync *el-get-packages-list*))

(defun el-get-install-packages ()
  "Install el-get"
  (message "Installing el-get...")
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/2.stable/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

;; ;; (el-get 'sync)

;;; initialize package
(add-to-list 'load-path (concat restaurant/source-directory "el-get/el-get"))

;;; initialize el-get
(unless (require 'el-get nil 'noerror)
  (el-get-install-packages))

;;; add user-defined recipes to el-get
(add-to-list 'el-get-recipe-path restaurant/elget-user-recipes-path)

;;; install package and el-get packages
(unless (file-exists-p restaurant/packages-installed-p)
  ;; el-get initialization
  ;; (init-elget-user-recipes)
  (init-el-get-packages)
  (write-region "" nil restaurant/packages-installed-p))

;;; Add local projects
(let ((default-directory (concat restaurant/source-directory "share/")))
  (when (not (file-directory-p default-directory))
    (make-directory default-directory))
  (setq load-path
        (append
         (let ((load-path (copy-sequence load-path))) ;; Shadow
           (normal-top-level-add-subdirs-to-load-path))
         load-path)))

;;; Waiting for installation completed
(when (and (not (null (process-list))) restaurant/do-bootstrap)
  (message "Waiting while installation completed...")
  (sleep-for 30))
;; ;; awaiting
;; (loop
;;  t
;;  (if (null (process-list))
;;      (return)
;;    ;; (sleep-for 10)))
;;    (and  (sleep-for 10) (message "...still running. Waiting 10 sec..."))))
