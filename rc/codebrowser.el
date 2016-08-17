;;;
;;; ecb
;;;
(when (not restaurant/code-browser-switch-to-simple)
  (require 'ecb)

  ;;;; defining standard layouts
  (ecb-layout-define "restaurant-3-1" left-right
		     "This function creates the following layout: 
 
   -------------------------------------------------------------- 
   |              |                               |             | 
   |  Methods     |                               | Directories | 
   |              |                               |             | 
   |              |                               |             | 
   |              |                               |             | 
   |              |                               |             | 
   |              |                               |             | 
   |--------------|             Edit              |             | 
   |              |                               |             | 
   |  Sources     |                               |             | 
   |              |                               |             | 
   |--------------|                               |             | 
   |              |                               |             | 
   |  History     |                               |             | 
   |              |                               |             | 
   -------------------------------------------------------------- 
   |                                                            | 
   |                    Compilation                             | 
   |                                                            | 
   -------------------------------------------------------------- 
 
If you have not set a compilation-window in `ecb-compile-window-height' then 
the layout contains no persistent compilation window and the other windows get a 
little more place."
		     (ecb-set-methods-buffer)
		     (ecb-split-ver 0.4)
		     (ecb-set-sources-buffer)
		     (ecb-split-ver 0.5)
		     (ecb-set-history-buffer)
		     (select-window (next-window (next-window)))
		     (ecb-set-directories-buffer)
		     (select-window (previous-window (selected-window) 0)))

  (ecb-layout-define "restaurant-2-2" left-right
		     "This function creates the following layout: 
 
   -------------------------------------------------------------- 
   |              |                               |             | 
   |  Methods     |                               | Directories | 
   |              |                               |             | 
   |              |                               |             | 
   |              |                               |             | 
   |              |                               |             | 
   |              |                               |             | 
   |--------------|             Edit              |-------------| 
   |              |                               |             | 
   |  History     |                               |  Sources    | 
   |              |                               |             | 
   |              |                               |             | 
   |              |                               |             | 
   |              |                               |             | 
   |              |                               |             | 
   -------------------------------------------------------------- 
   |                                                            | 
   |                    Compilation                             | 
   |                                                            | 
   -------------------------------------------------------------- 
 
If you have not set a compilation-window in `ecb-compile-window-height' then 
the layout contains no persistent compilation window and the other windows get a 
little more place."
		     (ecb-set-methods-buffer)
		     (ecb-split-ver 0.5)
		     (ecb-set-history-buffer) 
		     (select-window (next-window (next-window)))
		     (ecb-set-directories-buffer) 
		     (ecb-split-ver 0.66)
		     (ecb-set-sources-buffer)
		     (select-window (previous-window (previous-window (selected-window) 0) 0)))

  (custom-set-variables
   '(ecb-tip-of-the-day nil)
   '(ecb-options-version "2.40")
   '(ecb-layout-name "restaurant-2-2")
   '(ecb-windows-width 0.2)
   '(ecb-tip-of-the-day nil)
   '(ecb-compile-window-height 0.15)
   '(ecb-compile-window-width 'edit-window)
   '(ecb-compile-window-temporally-enlarge 'after-selection) ;; after-display after-selection both nil
   '(enlarged-compilation-window-max-height 'half) ;; best half
   '(ecb-create-layout-file (locate-user-config-file "ecb-user-layouts.el"))
   '(ecb-tip-of-the-day-file (locate-user-config-file "ecb-tip-of-day.el"))
   '(ecb-source-path (quote (("/" "Root"))))
   
   ;; ecb-windows-width 30
   ;; ecb-fix-window-size 'width
   ;; ecb-history-make-buckets 'mode
   ;; ecb-kill-buffer-clears-history 'auto
   ;; ecb-create-layout-frame-height 40
   ;; ecb-create-layout-frame-width 110
   '(ecb-primary-secondary-mouse-buttons 'mouse-1--mouse-2)
   ;; '(semantic-decoration-styles (list '("semantic-decoration-on-includes" . t)
   ;; 				      '("semantic-tag-boundary" . t))
   ;; 				;; ecb-create-layout-frame-height 40
   ;; 				;; ecb-create-layout-frame-width 110
   ;; 				))

   ;; (defconst initial-frame-width (frame-width)
   ;;   "The width of frame will be changed ,remember the init value.")
   
   ;; (add-to-list 'ecb-compilation-buffer-names '("*slime-repl sbcl*"))
   ;; ;;(add-to-list 'ecb-source-path  '("~/Git Repositories/Workspaces" "/root"))

   ;; (add-hook 'ecb-show-ecb-windows-before-hook
   ;;           'ecb-enlarge-frame-width-before-show)
   ;; (add-hook 'ecb-hide-ecb-windows-before-hook
   ;;           'ecb-shrink-frame-width-before-hide)
   ;; (add-hook 'ecb-deactivate-hook
   ;;           'ecb-shrink-frame-width-before-hide)
   ;; (add-hook 'ecb-activate-before-layout-draw-hook
   ;;           'ecb-enlarge-frame-width-before-activate)

   ;; (defun frame-horizontal-maximized-p ()
   ;;   "Test current frame wheather be maxmized by test the frame width and height equal to the screen resolution"
   ;;   (interactive)
   ;;   (equal (frame-pixel-width) (display-pixel-width)))

   ;; (defun ecb-enlarge-frame-width-before-show ()
   ;;   "Enlarge frame width before ecb shows layout."
   ;;   (if (and (ecb-windows-all-hidden)
   ;;            (<= (+ (frame-pixel-width) (* (frame-char-width)
   ;;                                          (+ ecb-windows-width 2)))
   ;;                (display-pixel-width)))
   ;;       (set-frame-width (selected-frame) (+ (frame-width) (+ ecb-windows-width 2)))))
   ;; (defun ecb-shrink-frame-width-before-hide ()
   ;;   "Shrink frame width before ecb hide layout."
   ;;   (if (and (not (ecb-windows-all-hidden))

   ;;            (not (eq (frame-pixel-width)
   ;;                     (display-pixel-width))))
   ;;       (if (< (- (frame-width) (+ ecb-windows-width 2)) initial-frame-width)
   ;;           (set-frame-width (selected-frame) initial-frame-width)
   ;;         (set-frame-width (selected-frame) (- (frame-width) (+ ecb-windows-width 2))))))
   ;; (defun ecb-enlarge-frame-width-before-activate ()
   ;;   "Enlarge frame width when ecb active and need it to."
   ;;   (let ((use-last-win-conf (and ecb-last-window-config-before-deactivation
   ;;                                 (equal ecb-split-edit-window-after-start
   ;;                                        'before-deactivation)
   ;;                                 (not (ecb-window-configuration-invalidp
   ;;                                       ecb-last-window-config-before-deactivation)))))
   ;;     (unless (or (and use-last-win-conf
   ;;                      (eq (nth 5 ecb-last-window-config-before-deactivation)
   ;;                          ecb-windows-hidden-all-value))
   ;;                 (> (+ (frame-pixel-width) (* (frame-char-width)
   ;;                                              (+ ecb-windows-width 2)))
   ;;                    (display-pixel-width)))
   ;;       (set-frame-width (selected-frame) (+ (frame-width) (+ ecb-windows-width 2))))))

   ;; (defadvice ecb-activate (after ecb-activate-after activate)
   ;;   "Redraw layout after activation of ecb."
   ;;   (ecb-redraw-layout))
   ;; (call-interactively 'ecb-activate)
   ))

;; (setq ecb-create-layout-file (locate-user-config-file "ecb-user-layouts.el"))

(when (not restaurant/code-browser-switch-to-simple)
  (custom-set-variables
   '(ecb-layout-name "restaurant-2-2")))

(when restaurant/code-browser-switch-to-simple
  (require 'sr-speedbar)
  (custom-set-variables
   '(speedbar-use-images t)
   '(sr-speedbar-right-side nil)
   '(speedbar-show-unknown-files t))
  )

(defun toggle-code-browser ()
  (interactive)
  (if restaurant/code-browser-switch-to-simple
      (sr-speedbar-toggle)
    (or (ecb-toggle-ecb-windows)
	(call-interactively 'ecb-activate))))

(global-set-key (kbd "<f11>") 'toggle-code-browser)

(add-hook 'emacs-startup-hook '(lambda ()
				 (if restaurant/code-browser-switch-to-simple
				     (sr-speedbar-open)
				   (call-interactively 'ecb-activate))))

;;;;; config-ecb.el ends here ---
