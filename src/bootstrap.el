;;; bootstrap.el --- make Restaurant bootstrap and early boot  -*- lexical-binding: t -*-

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

;; (defvar restaurant/source-directory (file-name-directory load-file-name))
(defvar restaurant/source-directory (file-name-directory (directory-file-name (file-name-directory load-file-name))))
(defvar restaurant/packages-installed-p (concat restaurant/source-directory "el-get/.installed-p"))
(defvar restaurant/elget-user-recipes-path (concat restaurant/source-directory "el-get-user/recipes"))
(defvar restaurant/do-bootstrap t)
(defvar restaurant/el-get-force-reinit nil)

(setq user-emacs-directory restaurant/source-directory)

(defvar *el-get-packages-list*
  '(
    ;; libraries and frameworks
    "f"
    "s"
    "apel"
    "cl-lib"
    "dash"
    "deferred"
    "emacs-async"
    "ht"
    "highlight-symbol"
    ;; ruby
    "ruby-mode"
    "enh-ruby-mode"
    "ruby-electric"
    "ruby-hash-syntax"
    "robe-mode"
    "ruby-block"
    "ruby-tools"
    "ruby-refactor"
    "rubocop"
    "rvm"
    "yari"
    "bundler"
    ;; python
    "python"
    "python-mode"
    "pymacs"
    "python-environment"
    "company-jedi"
    "autopair"
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
    "json-reformat"
    ;; xml
    ;; web
    "web-mode"
    "web-completion-data"
    "mmm-mode"
    ;; terraform
    "terraform-mode"
    ;; jenkins
    "jenkins"
    ;; docker
    "dockerfile-mode"
    ;; helm
    "helm"
    "helm-company"
    "helm-dictionary"
    "helm-gtags"
    "helm-ipython"
    "helm-projectile"
    "helm-pydoc"
    "helm-robe"
    "helm-shell"
    "helm-shell-history"
    "helm-themes"
    "helm-swoop"
    ;; other
    "ecb"
    "sr-speedbar"
    "magit"
    "gh" ;; needed for magit-gh-pulls
    ;; "magit-gh-pulls" moved to scripts
    "fill-column-indicator"
    "highlight-parentheses"
    "col-highlight"
    "flycheck"
    "flycheck-pos-tip"
    "flymake-ruby"
    "tabbar"
    "yasnippet"
    "yasnippets"
    ;; "yasnippet-snippets" ;; yasnippet/snippets are the same
    "projectile"
    "drag-stuff" ;; move lines/regions up/down
    "ssh"
    ;; notifications support
    "notify"
    ;; fun
    "nyan-mode"
    ;; themes
    "color-theme"
    "solarized-emacs"
    ))

(defun init-elget-user-recipes ()
  (message "init-elget-user-recipes")
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
(unless (and (file-directory-p restaurant/packages-installed-p)
	     (not restaurant/el-get-force-reinit))
  ;; el-get initialization
  ;; (init-elget-user-recipes)
  (message "Getting el-get packages...")
  (init-el-get-packages)
  (message "...Getting el-get packages [ DONE ]")
  (write-region "" nil restaurant/packages-installed-p))
   

;;; Add 3rd party projects
(let ((default-directory (concat restaurant/source-directory "lib/")))
  (when (file-directory-p default-directory)
    (setq load-path
          (append
           (let ((load-path (copy-sequence load-path))) ;; Shadow
             (normal-top-level-add-subdirs-to-load-path))
           load-path))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bootstrap.el ends here
