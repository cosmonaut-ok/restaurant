;;; Package --- Restaurant

;;; Commentary:

;;; Code:

(require 'markdown-mode)

(add-auto-mode 'markdown-mode
               "\\.md\\'" "\\.markdown\\'")

(add-hook 'markdown-mode-hook
	  (lambda ()
	    (when buffer-file-name
	      (add-hook 'before-save-hook
			'check-parens
			nil t))))

;; The Markdown files I write using IA Writer use newlines to separate
;; paragraphs. That's why I need Visual Line Mode. I also need to
;; disable M-q. If I fill paragraphs, that introduces unwanted
;; newlines.
(defun restaurant/markdown-config ()
  (local-set-key (kbd "M-q") 'ignore))

(add-hook 'markdown-mode-hook 'visual-line-mode)
(add-hook 'markdown-mode-hook 'restaurant/markdown-config)

