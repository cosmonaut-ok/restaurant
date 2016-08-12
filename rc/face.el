;;;
;;; toplevel
;;;
(defgroup restaurant nil
  "Customization for ``Restaurant`` IDE, based on Emacs."
  :group 'emacs)

;;;
;;; face
;;;
(defgroup restaurant/face nil
  "Color schemas, fonts etc"
  :group 'restaurant
  )

(defcustom restaurant/enable-tabbar t
  "Automatically activate code browser, when program file opens"
  :type 'boolean
  :group 'restaurant/face
  )

(defcustom restaurant/familiar-copy-paste-cut t
  "Bind familiar Control+c, Control+x and Control+v, instead emacs default keybindings"
  :type 'boolean
  :group 'restaurant/face
  )

(defcustom restaurant/fill-column t
  "Automatically activate code browser, when program file opens"
  :type 'boolean
  :group 'restaurant/face
  )

(defcustom restaurant/why-so-serious t
  "Lets enable some fun"
  :type 'boolean
  :group 'restaurant/face
  )

;;;
;;; backup
;;;
(defgroup restaurant/backup nil
  "Miscelation parameters"
  :group 'restaurant
  )

(defcustom restaurant/autobackup t
  "Automatically activate code browser, when program file opens"
  :type 'boolean
  :group 'restaurant/backup
  )

(defcustom restaurant/backup-directory t
  "Automatically activate code browser, when program file opens"
  :type 'boolean
  :group 'restaurant/backup
  )

;;;
;;; misc
;;;
;; (defgroup restaurant/misc nil
;;   "Miscelation parameters"
;;   :group 'restaurant
;;   )

;;;
;;; programming mode
;;;
(defgroup restaurant/programming nil
  "Customization for ``Restaurant`` IDE, based on Emacs."
  :group 'restaurant)

(defcustom restaurant/max-line-length 80
  "Defines maximum recommended line length in program"
  :type 'integer
  :group 'restaurant/programming
  )

(defcustom restaurant/indent-level 2
  "Defines maximum recommended line length in program"
  :type 'integer
  :group 'restaurant/programming
  )

(defcustom restaurant/indent-tabs-mode nil
  "Defines maximum recommended line length in program"
  :type 'boolean
  :group 'restaurant/programming
  )

(defcustom restaurant/indicate-parentheses t
  "Show pair to current parenth and highlight other parenthesis"
  :type 'boolean
  :group 'restaurant/programming
  )

(defcustom restaurant/linum-mode t
  "Defines maximum recommended line length in program"
  :type 'boolean
  :group 'restaurant/programming
  )

(defcustom restaurant/require-final-newline t
  "Defines maximum recommended line length in program"
  :type 'boolean
  :group 'restaurant/programming
  )

(defcustom restaurant/highlight-current-line t
  "Highlight current line"
  :type 'boolean
  :group 'restaurant/programming
  )

(defcustom restaurant/highlight-current-column nil
  "Highlight current column"
  :type 'boolean
  :group 'restaurant/programming
  )

(defcustom restaurant/enable-hide-show-blocks t
  "Automatically activate code browser, when program file opens"
  :type 'boolean
  :group 'restaurant/programming
  )

(defcustom restaurant/enable-code-browser t
  "Automatically activate code browser, when program file opens"
  :type 'boolean
  :group 'restaurant/programming
  )

(defcustom restaurant/enable-file-browser nil
  "Automatically activate code browser, when program file opens"
  :type 'boolean
  :group 'restaurant/programming
  )

(defcustom restaurant/enable-flycheck t
  "Enable on-the-fly style checking (used with robe, rubocop and foodcritic)"
  :type 'boolean
  :group 'restaurant/programming
  )

(defcustom restaurant/enable-flymake t
  "Enable flymake support (on-the-fly syntax checking)"
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
  "Enable electric mode - fast block completion"
  :type 'boolean
  :group 'restaurant/ruby
  )

(defcustom restaurant/enable-robe t
  "Enable ROBE automatically, when open ruby file"
  :type 'boolean
  :group 'restaurant/ruby
  )

(defcustom restaurant/enable-ruby-tools t
  "Enable ROBE automatically, when open ruby file"
  :type 'boolean
  :group 'restaurant/ruby
  )

(defcustom restaurant/enable-ruby-refactor t
  "Enable ROBE automatically, when open ruby file"
  :type 'boolean
  :group 'restaurant/ruby
  )

(defcustom restaurant/enable-rvm t
  "Enable ROBE automatically, when open ruby file"
  :type 'boolean
  :group 'restaurant/ruby
  )

(defcustom restaurant/enable-rubocop t
  "Enable ROBE automatically, when open ruby file"
  :type 'boolean
  :group 'restaurant/ruby
  )

(defcustom restaurant/enable-ri t
  "Ruby RI documentation support (press F1 on symbol to get it)"
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
  "Enable chef extensions, when using ruby files"
  :type 'boolean
  :group 'restaurant/chef
  )

(defcustom restaurant/enable-foodcritic t
  "Enable flycheck foodcritic checker (reboot needed)"
  :type 'boolean
  :group 'restaurant/chef
  )

;; (defcustom restaurant/test-kitchen-verbose-level "info"
;;   "Set test kitchen debug level (not implemented yet)"
;;   :type 'string
;;   :group 'restaurant/chef
;;   )
