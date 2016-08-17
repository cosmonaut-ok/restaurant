;;; user-directories.el  -*- lexical-binding: t -*-
;;
;; Copyright (C) 2016 Alexander aka 'CosmonauT' Vynnyk
;; Copyright (C) 2010--2012 Francisco Miguel ColaÃ§o
;;
;; Original Authors: Francisco Miguel Pedroso HonÃ³rio ColaÃ§o
;; Created: October 14, 1991
;; Maintainers: Francisco Miguel ColaÃ§o <address@hidden>
;;
;; Keywords: user configuration data directories
;; Summary: Defines user directories according to the XDG Base
;; Directory Specification.
;;
;; * DESCRIPTION
;;
;; user-directories, provided by this file, allows the user to find
;; and locate restaurant related files in directories which conform to the
;; XDG Base Directory Specification.
;;
;; These functions allow one to insert snippets as:
;;
;;   (setq config-file (locate-user-config-file "custom.el"))
;;
;; or:
;;
;;   (dolist (module (list "functions"
;;                    (format "restaurant-version-%d" restaurant-major-version)
;;                    (format "system-type-%s" (subst-char-in-string ?/ ?- (symbol-name system-type)))
;;                    (format "system-name-%s" system-name)
;;                    (format "window-system-%s" (symbol-name window-system))
;;                    "edit" "prog" "tex" "xml" "frame" "keys"))
;;      (load (locate-user-config-file (format "conf-%s" module)) t))
;;
;; The user should call (locate-user-<category>-file filename), or opt
;; instead to call (locate-user-file filename :<category>).  The
;; category must be one of :data :config :cache :runtime :lisp or
;; :documents, defaulting to :data when ommited.
;;
;; restaurant/user-lisp-directory is added to load-path.
;;
;; * USAGE
;;
;; Put the file in a directory belonging to the restaurant load path and
;; then include in your restaurant file:
;;
;;   (load-library "user-directories")
;;
;; As soon as this package it is loaded, you may locate files with the
;; included functions.  For instance:
;;
;;   (setq ecb-tip-of-the-day-file (locate-user-data-file "ecb-tip-of-the-day")
;;         nnmail-message-id-cache-file (locate-user-cache-file "nnmail")
;;
;; * RATIONALE
;;
;; The monolithic approach of having all under ~/.restaurant.d does suffer
;; from one drawback: when backing up, a lot of undesired cache data
;; will be present in the backup.  The XDG Base Directory
;; Specification addresses this problem by separating within different
;; directories the different data domains.  A simple and
;; straightforward way is needed for the restaurant user to locate files in
;; user restaurant-related directories (mostly within $HOME).
;;
;; * COPYRIGHT NOTICE
;;
;; Copyright (C) 2010-2012 Francisco Miguel Pedroso HonÃ³rio ColaÃ§o
;; Completion (C) 2016 Alexander aka 'CosmonauT' Vynnyk <cosmonaut.ok@zoho.com>
;;
;; This file is free software, licensed under the GNU Public License,
;; as published by the Free Software Foundation (FSF), at the version
;; 3 or above, at your choice.
;;
;; Please see http://www.gnu.org/copyleft/gpl.html for a copy of the
;; GNU General public License.
;;
;; * AUTHORS
;;
;; Francisco Miguel ColaÃ§o <address@hidden>
;; Alexander aka 'CosmonauT' Vynnyk <cosmonaut.ok@zoho.com>
;;
;; * IMPROVEMENTS TO CONSIDER
;;
;;   - Make sensible defaults for MS Windows, using the directory
;;     structure within %PROFILE% or whatever, or a structure like
;;     ~/.restaurant/config, ~/.restaurant/cache, etc.  What about other
;;     non-posix environments?
;;

(eval-when-compile
  (require 'cl))

(defun user-get-restaurant-directory (variable fallback)
  "Get an restaurant given an environment variable VARIABLE and a
FALLBACK path.  The returned file name is abbreviated.

See user-get-directory-from-environment."
  (abbreviate-file-name
   (expand-file-name "restaurant" (or (getenv variable) fallback))))

;;; Custom.
;;;
(defgroup restaurant/user-directories nil
  "Directories used to classify and store the user documents."
  :group 'restaurant)

(defcustom restaurant/user-config-directory
  (user-get-restaurant-directory "XDG_CONFIG_HOME" "~/.config/")
  "The user directory where configuration files are stored."
  :type 'string :group 'restaurant/user-directories)

(defcustom restaurant/user-data-directory
  (user-get-restaurant-directory "XDG_DATA_HOME" "~/.local/share/")
  "The user directory where data files are stored."
  :type 'string :group 'restaurant/user-directories)

(defcustom restaurant/user-cache-directory
  (user-get-restaurant-directory "XDG_CACHE_HOME" "~/.cache/")
  "The user directory where cache files are stored."
  :type 'string :group 'restaurant/user-directories)

(defcustom restaurant/user-runtime-directory
  (user-get-restaurant-directory "XDG_RUNTIME_DIR" "/tmp/")
  "The user directory where files that belong to just one session are stored."
  :type 'string :group 'restaurant/user-directories)

(defcustom restaurant/user-lisp-directory
  (expand-file-name "lisp" restaurant/user-data-directory)
  "The user directory where files that belong to just one session are stored."
  :type 'string :group 'restaurant/user-directories)

(defcustom restaurant/user-documents-directory
  (if (zerop (call-process "xdg-user-dir" nil nil nil))
      (remove ?\n (shell-command-to-string "xdg-user-dir \"DOCUMENTS\""))
    (expand-file-name "~/Documents"))
  "The user directory where the user stores it's documents."
  :type 'string :group 'restaurant/user-directories)

;; User restaurant directory is defconst'd.  But assigning it works, since
;; restaurant lisp does not enforce constants.
(setq user-emacs-directory restaurant/user-data-directory)
(add-to-list 'load-path restaurant/user-lisp-directory)

;;; Functions to locate files within the user restaurant directories.
;;;
;;; These functions will always return something (or an error), so
;;; there is no need for fallback filenames.
;;;

;; (:fhc: for some obscure aesthetic reason, I prefer keyworded
;; symbols.  It this violates the written or unwritten restaurant
;; conventions, please feel free to alter it, as I seldom use
;; locate-user-file outside this file.)
(defun locate-user-file (filename &optional directory)
  "Locates FILENAME in an restaurant user directory.

DIR can be one of :data :config :cache :lisp :runtime :documents.
It defaults to :data, in which case will locate the file within
restaurant/user-data-directory."
  (let ((dir (case directory
                   ((nil :data) restaurant/user-data-directory)
                   (:config restaurant/user-config-directory)
                   (:cache restaurant/user-cache-directory)
                   (:runtime restaurant/user-cache-directory)
                   (:lisp restaurant/user-lisp-directory)
                   (:documents restaurant/user-documents-directory)
		   (:source restaurant/source-directory) ;; look in ../init.el
                   (t (error "Directory must be ommited or one of :data :config :cache :lisp :runtime :documents :source.")))))
    (expand-file-name filename dir)))


(defun locate-user-config-file (filename)
  "Locates FILENAME in the user restaurant config directory."
  (locate-user-file filename :config))


(defun locate-user-data-file (filename)
  "Locates FILENAME in the user restaurant data directory."
  (locate-user-file filename :data))

(defun locate-user-cache-file (filename)
  "Locates FILENAME in the user restaurant cache directory."
  (locate-user-file filename :cache))

(defun locate-user-lisp-file (filename)
  "Locates FILENAME in the user restaurant lisp directory."
  (locate-user-file filename :lisp))

(defun locate-source-file (filename)
  "Locates FILENAME in the restaurant source directory."
  (locate-user-file filename :source))

;; (defun locate-user-emacs-file (filename &optional old-filename)
;;   "Locates FILENAME in the restaurant data directory."
;;   (locate-user-file filename :config))

(defun add-to-path (dir &optional append)
  "Adds DIR to path."
  (add-to-list 'load-path dir append))

(defun add-user-lisp-to-path (dir &optional append)
  "Adds DIR inside user-lisp-directory to path."
  (add-to-path (locate-user-lisp-file dir) append))

(provide 'user-directories)
