;;;
;;; clearing default menu
;;;
(define-key global-map [menu-bar tools games] nil) ;; TODO: enable in why-so-serious mode :)
(define-key prog-mode-map [menu-bar hide-show] nil) ;; not working for some reason
(define-key global-map [menu-bar tools compare] nil)
(define-key global-map [menu-bar tools ediff-merge] nil)
(define-key global-map [menu-bar tools epatch] nil)
(define-key global-map [menu-bar tools compose-mail] nil)
(define-key global-map [menu-bar tools gnus-menu-item] nil)
(define-key global-map [menu-bar tools gnus] nil)
(define-key global-map [menu-bar tools rmail] nil)
(define-key global-map [menu-bar tools vc] nil)
(define-key global-map [menu-bar tools shell] nil)
(define-key global-map [menu-bar tools gdb] nil)
(define-key global-map [menu-bar tools compile] nil)
(define-key global-map [menu-bar tools Color\ Themes] nil)
(define-key global-map [menu-bar tools Start\ Code\ Browser\ \(ECB\)] nil)
;; (define-key global-map [menu-bar options package] nil) ;; TODO: do something with this
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key enh-ruby-mode-map
  [menu-bar project]
  (cons "Project" (make-sparse-keymap "Project")))

;;;
;;; projectile
;;;
(define-key enh-ruby-mode-map
  [menu-bar project search-in-project]
  '("Search in Current Project" . projectile-grep))

(define-key enh-ruby-mode-map
  [menu-bar project open-project-file]
  '("Open Project File" . projectile-find-file-dwim))

(define-key enh-ruby-mode-map
  [menu-bar project project-run-shell-command-in-root]
  '("Run shell command in project root directory" . projectile-run-async-shell-command-in-root))

;;;
;;; Restaurant legacy menu
;;;
(define-key global-map [menu-bar options restaurant]
  (cons "Restaurant" (make-sparse-keymap "Restaurant")))

(define-key global-map [menu-bar options restaurant customize-restaurant]
  (cons "Customize Restaurant" (make-sparse-keymap "Customize Restaurant")))

(define-key global-map
  [menu-bar options restaurant customize-restaurant restaurant/customize]
  '("Restaurant Customization Menu" . restaurant/customize))

(define-key global-map
  [menu-bar options restaurant customize-restaurant customize-themes]
  '("Customize Themes" . customize-themes))

(define-key global-map
  [menu-bar options restaurant customize-restaurant customize]
  '("Global Customization Menu" . customize))

(define-key global-map
  [menu-bar options restaurant hide-all-blocks]
  '("Hide all code blocks" . hs-hide-all))

(define-key global-map
  [menu-bar options restaurant show-all-blocks]
  '("Show all code blocks" . hs-show-all))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; new menu
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key-after prog-mode-map
  [menu-bar code]
  (cons "Code" (make-sparse-keymap "Code")) 'options)

(define-key prog-mode-map
  [menu-bar code activate-codebrowser]
  '("Start Code Browser (ECB)" . ecb-activate))

(define-key prog-mode-map
  [menu-bar code codebrowser-separator]
  '("--" nil :visible))

(define-key prog-mode-map
  [menu-bar code toggle-codebrowser]
  '("Toggle Code Browser Windows" . toggle-code-browser))

;; Submap
(define-key prog-mode-map
  [menu-bar code compare]
  (cons "Compare" menu-bar-ediff-menu))

(define-key prog-mode-map
  [menu-bar code merge]
  (cons "Merge" menu-bar-ediff-merge-menu))

(define-key prog-mode-map
  [menu-bar code epatch]
  (cons "Apply Patch" menu-bar-epatch-menu))

;;;
;;; rubocop
;;;
;; (add-hook 'chef-mode-hook #'restaurant/chef-kitchen)

;; add rubocop menu
(define-key enh-ruby-mode-map
  [menu-bar code syntax-correction]
  (cons "Syntax Correction" (make-sparse-keymap "Syntax Correction")))

(define-key enh-ruby-mode-map
  [menu-bar code syntax-correction check-current-file]
  '("Check Current File" . rubocop-check-current-file))

(define-key enh-ruby-mode-map
  [menu-bar code syntax-correction check-directory]
  '("Check Directory" . rubocop-check-directory))

(define-key enh-ruby-mode-map
  [menu-bar code syntax-correction check-project]
  '("Check Project" . rubocop-check-project))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key enh-ruby-mode-map
  [menu-bar code syntax-correction separator]
  '("--" nil :visible))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key enh-ruby-mode-map
  [menu-bar code syntax-correction autocorrect-current-file]
  '("Autocorrect Current File" . rubocop-autocorrect-current-file))

(define-key enh-ruby-mode-map
  [menu-bar code syntax-correction autocorrect-directory]
  '("Autocorrect Directory" . rubocop-autocorrect-directory))

(define-key enh-ruby-mode-map
  [menu-bar code syntax-correction autocorrect-project]
  '("Autocorrect Project" . rubocop-autocorrect-project))

(define-key prog-mode-map
  [menu-bar code expand]
  (cons "Expand" yas--minor-mode-menu))

(define-key prog-mode-map
  [menu-bar code complete]
  (cons "Complete" (make-sparse-keymap "Complete")))

(define-key prog-mode-map
  [menu-bar code comment-separator]
  '("--" nil :visible))

(define-key prog-mode-map
  [menu-bar code comment/uncomment-selected]
  '("Comment/Uncomment Selected" . comment-dwim))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Refactor
;;;

(define-key-after prog-mode-map
  [menu-bar refactor]
  (cons "Refactor" (make-sparse-keymap "Refactor")) 'options)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; VCS
;;;
(define-key prog-mode-map
  [menu-bar vcs]
  (cons "VCS" vc-menu-map))

;;;
;;; magit
;;;
;;; 
(define-key prog-mode-map
  [menu-bar vcs git]
  (cons "Git" (make-sparse-keymap "Git")))

(define-key prog-mode-map
  [menu-bar vcs git clone]
  '("Clone Git Repository" . magit-clone))

(define-key prog-mode-map
  [menu-bar vcs git magit-separator]
  '("--" nil :visible))

(define-key prog-mode-map
  [menu-bar vcs git push-to-upstream]
  '("Push current branch to remote" . magit-push-current-to-pushremote))

(define-key prog-mode-map
  [menu-bar vcs git commit]
  '("Commit Changed" . magit-commit))

(define-key prog-mode-map
  [menu-bar vcs git show-log-current]
  '("Log Current Branch" . magit-log-current))

(define-key prog-mode-map
  [menu-bar vcs git add-changed]
  '("Manage Changed" . magit-diff-unstaged))

(define-key prog-mode-map
  [menu-bar vcs git show-status]
  '("Status" . magit-status))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key prog-mode-map
  [menu-bar vcs github]
  (cons "GitHub" (make-sparse-keymap "GitHub")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key prog-mode-map
  [menu-bar vcs github create-pullrequest]
  '("Create pull request branch" . magit-gh-pulls-create-pullrequest))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key magit-mode-map
  [menu-bar github]
  (cons "GitHub" (make-sparse-keymap "GitHub")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key magit-mode-map
  [menu-bar github add-changed]
  '("Create pull request branch" . magit-gh-pulls-create-branch))

(define-key magit-mode-map
  [menu-bar github fetch-pr-commits]
  '("Fetch pull request commits" . magit-gh-pulls-fetch-commits))

(define-key magit-mode-map
  [menu-bar github open-pr-in-browser]
  '("Open PR In Web Browser" . magit-gh-pulls-open-in-browser))

(define-key magit-mode-map
  [menu-bar github show-log-current]
  '("Reload pull request" . magit-gh-pulls-reload))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key global-map
  [menu-bar options color-themes]
  '("Color Themes" . color-theme-select))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Run
;;;
(define-key prog-mode-map
  [menu-bar run]
  (cons "Run" (make-sparse-keymap "Run")))

;;;
;;; test-kitchen
;;;

(define-key enh-ruby-mode-map
  [menu-bar run test-kitchen]
  (cons "Test Kitchen" (make-sparse-keymap "Test Kitchen")))

(define-key enh-ruby-mode-map
  [menu-bar run test-kitchen destroy]
  '("Destroy" . test-kitchen-destroy))

(define-key enh-ruby-mode-map
  [menu-bar run test-kitchen kitchen-separator]
  '("--" nil :visible))

(define-key enh-ruby-mode-map
  [menu-bar run test-kitchen test]
  '("Test" . test-kitchen-test))

(define-key enh-ruby-mode-map
  [menu-bar run test-kitchen list]
  '("List" . test-kitchen-list))

(define-key enh-ruby-mode-map
  [menu-bar run test-kitchen verify]
  '("Verify" . test-kitchen-verify))

(define-key enh-ruby-mode-map
  [menu-bar run test-kitchen login]
  '("Login" . test-kitchen-login))

(define-key enh-ruby-mode-map
  [menu-bar run test-kitchen converge]
  '("Converge" . test-kitchen-converge))


;;;
;;; rspec
;;;
(define-key enh-ruby-mode-map
  [menu-bar run unit-testing]
  (cons "Unit Tests (RSpec)" (make-sparse-keymap "Unit Tests")))

(define-key enh-ruby-mode-map
  [menu-bar run unit-testing run-all-tests]
  '("Run all tests" . rspec-verify-all))

(define-key enh-ruby-mode-map
  [menu-bar run test-kitchen kitchen-separator]
  '("--" nil :visible))

(define-key enh-ruby-mode-map
  [menu-bar run unit-testing run-tests-current-file]
  '("Run all tests on current file" . rspec-verify))

(define-key enh-ruby-mode-map
  [menu-bar run unit-testing run-test-at-point]
  '("Run test at cursor" . rspec-verify-single))

;;;; GDB
(define-key prog-mode-map
  [menu-bar run gdb-separator]
  '("--" nil :visible))

(define-key prog-mode-map
  [menu-bar run gdb]
  '("Debug (GDB)" . gdb))

(define-key prog-mode-map
  [menu-bar run gdb]
  '("Compile" . compile))

;;;; shell command
(define-key prog-mode-map
  [menu-bar run shell-command-separator-1]
  '("--" nil :visible))

(define-key prog-mode-map
  [menu-bar run shell-command-on-selected]
  '("Selected as Shell Command" . shell-command-on-region))

(define-key prog-mode-map
  [menu-bar run shell-command]
  '("Shell Command" . shell-command))

(define-key prog-mode-map
  [menu-bar run shell-command-separator-2]
  '("--" nil :visible))

(define-key global-map
  [menu-bar tools ssh-session]
  '("SSH Session" . ssh))

(define-key global-map
  [menu-bar tools elisp-console]
  '("Interactive Scripting Console" . ielm))
