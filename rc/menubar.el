(define-key enh-ruby-mode-map
  [menu-bar project]
  (cons "Project" (make-sparse-keymap "Project")))

;;;
;;; test-kitchen
;;;

;; add kitchen menu
(define-key enh-ruby-mode-map
  [menu-bar project test-kitchen]
  (cons "Test Kitchen" (make-sparse-keymap "Test Kitchen")))

(define-key enh-ruby-mode-map
  [menu-bar project test-kitchen converge]
  '("Converge" . test-kitchen-converge))

(define-key enh-ruby-mode-map
  [menu-bar project test-kitchen list]
  '("List" . test-kitchen-list))

(define-key enh-ruby-mode-map
  [menu-bar project test-kitchen verify]
  '("Verify" . test-kitchen-verify))

(define-key enh-ruby-mode-map
  [menu-bar project test-kitchen destroy]
  '("Destroy" . test-kitchen-destroy))

(define-key enh-ruby-mode-map
  [menu-bar project test-kitchen test]
  '("Test" . test-kitchen-test))

(define-key enh-ruby-mode-map
  [menu-bar test-kitchen test]
  '("Login" . test-kitchen-login))

;;;
;;; rubocop
;;;
;; (add-hook 'chef-mode-hook #'restaurant/chef-kitchen)

;; add rubocop menu
(define-key enh-ruby-mode-map
  [menu-bar project syntax-correction]
  (cons "Syntax Correction" (make-sparse-keymap "Syntax Correction")))

(define-key enh-ruby-mode-map
  [menu-bar project syntax-correction check-current-file]
  '("Check Current File" . rubocop-check-current-file))

(define-key enh-ruby-mode-map
  [menu-bar project syntax-correction check-directory]
  '("Check Directory" . rubocop-check-directory))

(define-key enh-ruby-mode-map
  [menu-bar project syntax-correction check-project]
  '("Check Project" . rubocop-check-project))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key enh-ruby-mode-map
  [menu-bar project syntax-correction separator]
  '("--" nil :visible))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-key enh-ruby-mode-map
  [menu-bar project syntax-correction autocorrect-current-file]
  '("Autocorrect Current File" . rubocop-autocorrect-current-file))

(define-key enh-ruby-mode-map
  [menu-bar project syntax-correction autocorrect-directory]
  '("Autocorrect Directory" . rubocop-autocorrect-directory))

(define-key enh-ruby-mode-map
  [menu-bar project syntax-correction autocorrect-project]
  '("Autocorrect Project" . rubocop-autocorrect-project))

;;;
;;; rspec
;;;
(define-key enh-ruby-mode-map
  [menu-bar project unit-testing]
  (cons "Unit Testing" (make-sparse-keymap "Unit Testing")))

(define-key enh-ruby-mode-map
  [menu-bar project unit-testing run-test-at-point]
  '("Run test at cursor" . rspec-verify-single))

(define-key enh-ruby-mode-map
  [menu-bar project unit-testing run-tests-current-file]
  '("Run all tests on current file" . rspec-verify))

(define-key enh-ruby-mode-map
  [menu-bar project unit-testing run-all-tests]
  '("Run all tests" . rspec-verify-all))

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
