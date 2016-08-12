(require 'yaml-mode)

(add-hook 'yaml-mode-hook
	  (lambda ()
	    (define-key yaml-mode-map (kbd "RET") 'newline-and-indent)))
