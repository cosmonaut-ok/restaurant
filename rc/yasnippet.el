;;; rc-yasnippet.el ---

;; Copyright (C) Alexander Vynnyk
;;
;; Author: Alexander Vynnyk <cosmonaut.ok@zoho.com>

;;; use popup menu for yas-choose-value
(require 'popup)

(require 'yasnippet)
;; (require 'yasnippets)

(yas/initialize)

(add-to-list 'yas/snippet-dirs (concat restaurant/source-directory "/data/snippets/"))
(add-to-list 'yas/snippet-dirs (concat restaurant/source-directory "el-get/yasnippet-snippets/"))
(add-to-list 'yas/snippet-dirs (concat restaurant/source-directory "el-get/yasnippets/"))
(add-to-list 'yas/snippet-dirs (concat restaurant/source-directory "share/rspec-mode/snippets/"))
(add-to-list 'yas/snippet-dirs (concat restaurant/user-data-directory "/snippets/"))

;; (yas-global-mode 1)
(yas/reload-all)

;; do not bind yasnippet to TAB key. Bind it to C-TAB
(define-key yas-minor-mode-map (kbd "<C-M-tab>") 'yas-ido-expand)
(define-key yas-minor-mode-map (kbd "<tab>") nil)
(global-set-key (kbd "<C-tab>") 'company-yasnippet)
;; (add-to-list 'auto-mode-alist '("/snippets/" . snippet-mode))

(add-hook 'snippet-mode-hook (lambda ()
                               (font-lock-mode 1)))

;; add some shotcuts in popup menu mode
(define-key popup-menu-keymap (kbd "M-n") 'popup-next)
(define-key popup-menu-keymap (kbd "TAB") 'popup-next)
(define-key popup-menu-keymap (kbd "<tab>") 'popup-next)
(define-key popup-menu-keymap (kbd "<backtab>") 'popup-previous)
(define-key popup-menu-keymap (kbd "M-p") 'popup-previous)

(defun yas-popup-isearch-prompt (prompt choices &optional display-fn)
  (when (featurep 'popup)
    (popup-menu*
     (mapcar
      (lambda (choice)
	(popup-make-item
	 (or (and display-fn (funcall display-fn choice))
	     choice)
	 :value choice))
      choices)
     :prompt prompt
     ;; start isearch mode immediately
     :isearch t
     )))

(setq yas-prompt-functions '(yas-popup-isearch-prompt yas-ido-prompt yas-no-prompt))

;; Completing point by some yasnippet key
(defun yas-ido-expand ()
  "Lets you select (and expand) a yasnippet key"
  (interactive)
  (let ((original-point (point)))
    (while (and
	    (not (= (point) (point-min) ))
	    (not
	     (string-match "[[:space:]\n]" (char-to-string (char-before)))))
      (backward-word 1))
    (let* ((init-word (point))
	   (word (buffer-substring init-word original-point))
	   (list (yas-active-keys)))
      (goto-char original-point)
      (let ((key (remove-if-not
		  (lambda (s) (string-match (concat "^" word) s)) list)))
	(if (= (length key) 1)
	    (setq key (pop key))
	  (setq key (ido-completing-read "key: " list nil nil word)))
	(delete-char (- init-word original-point))
	(insert key)
	(yas-expand)))))

(eval-after-load 'rspec-mode
  '(rspec-install-snippets))

;; yas-chef-mode
(add-hook 'chef-mode-hook
	  #'(lambda ()
	      (yas-activate-extra-mode 'chef-mode)))

;; do not activate yas in term
(add-hook 'term-mode-hook (lambda()
			    (setq yas-dont-activate t)))

;;;
;;; yas-minor-mode
;;;
(defun restaurant/yas-minor-mode-init ()
  (yas-minor-mode-on)
  )

(add-hook 'prog-mode-hook 'restaurant/yas-minor-mode-init)

;;; yasnippet.el ends here
