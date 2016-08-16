(defun choose-from-menu (menu-title menu-items)
  "Choose from a list of choices from a popup menu.
See `popup-commands' which calls this"
  (let ((item)
	(item-list))
    (while menu-items
      (setq item (car menu-items))
      (if (consp item)
	  (setq item-list (cons (cons (car item) (cdr item) ) item-list))
	(setq item-list (cons (cons item item) item-list)))
      (setq menu-items (cdr menu-items))
      )

    (x-popup-menu t (list menu-title (cons menu-title (nreverse item-list))))))

(defun right-popup ()
  "Show a popup menu of commands. See also `choose-from-menu'."
  (interactive)
  (eval-expression
   (car
    (read-from-string
     (choose-from-menu
      "Appetizer"
      (list
       (cons "Cut [C-w]" "(call-interactively 'kill-region)")
       (cons "Copy [M-w]" "(call-interactively 'kill-ring-save)")
       (cons "Paste (Yank) [C-y]" "(yank)")
       (cons "Paste from history" "(popup-menu 'yank-menu)")
       (cons "-" "")
       (cons "Open Terminal here" "(open-console)")
       (cons "-" "")
       (cons "Undo [C-x u]" "(undo)")
       (cons "Redo" "(call-interactively 'redo)")
       (cons "-" "")
       (cons "Refactor N/I" "")
       (cons "Folding N/I" "")
       (cons "-" "")
       (cons "Save Current File [C-x C-s]" "(call-interactively 'save-buffer)")
       (cons "-" "")
       (cons "Search [C-s]" "(call-interactively 'search-forward)")
       (cons "Search [C-s-1]" (cons "zzz" "(call-interactively 'search-forward)"))
       (cons "-" "")
       (cons "Open buffer in new frame (WM window) [C-x 5 2]"
	     "(make-frame-command)")
       (cons "Hide buffer [C-x 0]"  "(delete-window)")
       (cons "-" "")
       (cons "-" "")
       (cons "Close buffer [C-x k]"  "(kill-buffer)")
       ))))))


;; (x-popup-menu
;;  t
;;  '("Title"
;;    ("Title 1" ("Item 1-1" . 11) ("Item 1-2" . 12))
;;    ("Title 2" ("Item 2-1" . 21) ("Item 2-2" . 22))))

(global-set-key [mouse-3] 'right-popup)

;;;; popup-menu.el ends here
