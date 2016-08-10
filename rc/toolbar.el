;; add
(define-key-after (default-value 'tool-bar-map) [separator-10] menu-bar-separator)
;;
(tool-bar-add-item "terminal" 'open-console 'open-terminal-here)
;;
(define-key-after (default-value 'tool-bar-map) [separator-11] menu-bar-separator)
;;
(tool-bar-add-item "hsplit" 'split-window-below 'split-window-horizontally)
(tool-bar-add-item "vsplit" 'split-window-right 'split-window-vertically)
(tool-bar-add-item "minimize" 'delete-window 'remove-split-and-hide-current-window)
(tool-bar-add-item "one" 'delete-other-windows 'leave-only-current-window)
;;
(define-key-after (default-value 'tool-bar-map) [separator-12] menu-bar-separator)
