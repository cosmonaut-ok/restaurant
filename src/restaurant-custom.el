;;; restaurant-custom.el --- Restaurant customizations  -*- lexical-binding: t -*-

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

;; Restaurant customizations

;;; Code:

;;;
;;; toplevel
;;;
(defgroup restaurant nil
  "Customization for ``Restaurant`` IDE, based on Emacs."
  :group 'emacs)

;;;
;;; system
;;;
(defgroup restaurant/system nil
  "Color schemas, fonts etc"
  :group 'restaurant
  )


(defcustom restaurant/enable-verbose nil
  "Enable verbose loading."
  :type 'boolean
  :group 'restaurant/system
  )

;;;
;;; face
;;;
(defgroup restaurant/face nil
  "Color schemas, fonts etc"
  :group 'restaurant
  )

(defcustom restaurant/enable-tabbar t
  "Automatically activate code browser, when program file opens."
  :type 'boolean
  :group 'restaurant/face
  )

(defcustom restaurant/familiar-copy-paste-cut t
  "Bind familiar Control+c, Control+x and Control+v, instead Emacs default keybindings."
  :type 'boolean
  :group 'restaurant/face
  )

(defcustom restaurant/fill-column t
  "Enable special highlighting of long lines."
  :type 'boolean
  :group 'restaurant/face
  )

(defcustom restaurant/notify-on-build t
  "Notify on build, compile or test process finished."
  :type 'boolean
  :group 'restaurant/face
  )

(defcustom restaurant/enable-github-flavored-preview nil
  "Enable markdown preview, generated with github (requires internet connection). Experimental!"
  :type 'boolean
  :group 'restaurant/face
  )

(defcustom restaurant/why-so-serious t
  "Lets enable some fun."
  :type 'boolean
  :group 'restaurant/face
  )

;;;
;;; backup
;;;
(defgroup restaurant/backup nil
  "Backup parameters"
  :group 'restaurant
  )

(defcustom restaurant/autobackup t
  "Enable automatic file backups."
  :type 'boolean
  :group 'restaurant/backup
  )

(defcustom restaurant/clear-autobackups nil
  "Clear old autobackup files during startup."
  :type 'boolean
  :group 'restaurant/backup
  )

(defcustom restaurant/autobackup-interval 300
  "Set interval between autobackups."
  :type 'integer
  :group 'restaurant/backup
  )

(defcustom restaurant/backup-directory (locate-user-data-file "backups/")
  "Enable backups." ;; FIXME: not working yet
  :type 'directory
  :group 'restaurant/backup
  )

;;;
;;; programming mode
;;;
(defgroup restaurant/programming nil
  "Customization for ``Restaurant`` IDE, based on Emacs."
  :group 'restaurant)

(defcustom restaurant/max-line-length 80
  "Defines maximum recommended line length in program."
  :type 'integer
  :group 'restaurant/programming
  )

(defcustom restaurant/highlight-too-long-lines t
  "Highlight lines, longer than maximum recommended length as ``warning``."
  :type 'boolean
  :group 'restaurant/programming
  )

(defcustom restaurant/indent-level 2
  "Define number of spaces to indent code."
  :type 'integer
  :group 'restaurant/programming
  )

(defcustom restaurant/indent-tabs-mode nil
  "Indent code by tabs (``false`` switches to spaces)."
  :type 'boolean
  :group 'restaurant/programming
  )

(defcustom restaurant/indicate-parentheses t
  "Show pair to current parenth and highlight other parenthesis."
  :type 'boolean
  :group 'restaurant/programming
  )

(defcustom restaurant/linum-mode t
  "Add lines numbering."
  :type 'boolean
  :group 'restaurant/programming
  )

(defcustom restaurant/require-final-newline t
  "Defines maximum recommended line length in program."
  :type 'boolean
  :group 'restaurant/programming
  )

(defcustom restaurant/highlight-current-line t
  "Highlight current line."
  :type 'boolean
  :group 'restaurant/programming
  )

(defcustom restaurant/highlight-current-column nil
  "Highlight current column."
  :type 'boolean
  :group 'restaurant/programming
  )

(defcustom restaurant/enable-hide-show-blocks t
  "Enable possibility to hide/show code blocks, like functions, classes etc."
  :type 'boolean
  :group 'restaurant/programming
  )

(defcustom restaurant/code-browser-switch-to-simple nil
  "Switch to simple directory navigaton ``false`` enables standard restaurant code browser."
  :type 'boolean
  :group 'restaurant/programming
  )

(defcustom restaurant/enable-flycheck t
  "Enable on-the-fly style checking (used with robe, rubocop and foodcritic)."
  :type 'boolean
  :group 'restaurant/programming
  )

(defcustom restaurant/flycheck-disabled-checkers '(ruby-rubylint)
  "Disable some flycheck checkers for ruby files."
  :type 'list
  :group 'restaurant/programming
  )

;; (defcustom restaurant/enable-flymake t
;;   "Enable flymake support (on-the-fly syntax checking)."
;;   :type 'boolean
;;   :group 'restaurant/programming
;;   )

(defcustom restaurant/enable-spell-checking nil
  "Enable spell checking in code comments."
  :type 'boolean
  :group 'restaurant/programming
  )
;;;
;;; ruby
;;;
(defgroup restaurant/ruby nil
  "Ruby-specific options"
  :group 'restaurant
)

(defcustom restaurant/enable-electric t
  "Enable ruby electric (fast parenth and block completions)."
  :type 'boolean
  :group 'restaurant/ruby
  )

(defcustom restaurant/enable-robe t
  "Enable ROBE automatically, when open ruby file."
  :type 'boolean
  :group 'restaurant/ruby
  )

(defcustom restaurant/enable-ruby-tools t
  "Enable ruby tools automatically, when open ruby file."
  :type 'boolean
  :group 'restaurant/ruby
  )

(defcustom restaurant/enable-ruby-refactor t
  "Enable ruby refactor automatically, when open ruby file."
  :type 'boolean
  :group 'restaurant/ruby
  )

(defcustom restaurant/enable-rvm t
  "Use RVM (if possible)."
  :type 'boolean
  :group 'restaurant/ruby
  )

(defcustom restaurant/rvm-default-gemset (or
					  (get-value-by-key-from-file
                                           "GEMSET_NAME"
                                           (locate-source-file "etc/restaurant.conf"))
					  "global")
  "Default rvm default gemset."
  :type 'string
  :group 'restaurant/ruby)

(defcustom restaurant/enable-rubocop t
  "Enable rubocop, when open ruby file."
  :type 'boolean
  :group 'restaurant/ruby
  )

(defcustom restaurant/enable-ri t
  "Ruby RI documentation support (press F1 on symbol to get it)."
  :type 'boolean
  :group 'restaurant/ruby
  )

;;;
;;; chef
;;;
(defgroup restaurant/chef nil
  "Color schemas, fonts etc"
  :group 'restaurant
  )

(defcustom restaurant/enable-chef t
  "Enable chef extensions, when using ruby files."
  :type 'boolean
  :group 'restaurant/chef
  )

(defcustom restaurant/enable-foodcritic t
  "Enable flycheck foodcritic checker (reboot needed)."
  :type 'boolean
  :group 'restaurant/chef
  )

(defcustom restaurant/enable-chefdk t
  "Enable chef extensions, when using ruby files."
  :type 'boolean
  :group 'restaurant/chef
  )

(defcustom restaurant/chefdk-home "/opt/chefdk"
  "Set chefDK home directory."
  :type 'directory
  :group 'restaurant/chef
  )

(defcustom restaurant/enable-bundler t
  "Enable bundler, when using ruby files."
  :type 'boolean
  :group 'restaurant/chef
  )

;;;
;;; 3rd-party important modes
;;;
(defgroup restaurant/misc nil
  "Not Restaurant-related important ruby, chef etc. options"
  :group 'restaurant
  )

(defgroup rspec-mode nil
  "RSpec minor mode."
  :group 'restaurant/misc)

(defgroup enh-ruby nil
  "Ruby mode."
  :group 'restaurant/misc)

(defgroup chef-mode nil
  "Chef minor mode."
  :group 'restaurant/misc)

(defgroup github-notifier nil
  "Chef minor mode."
  :group 'restaurant/misc)

;; add button open terminal here
(defcustom restaurant/terminal-emulator (get-terminal-emulator)
  "Default terminal emulator."
  :type 'string
  :group 'restaurant/system)

(defcustom restaurant/use-external-terminal-emulator nil
  "User external terminal emulator, instead of standard restaurant's shell."
  :type 'boolean
  :group 'restaurant/system)

(defcustom restaurant/check-parens-before-save nil
  "Check if all parens are paired before file saving."
  :type 'boolean
  :group 'restaurant/system)

;;; restaurant-custom.el ends here
