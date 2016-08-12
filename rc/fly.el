;;;
;;; flycheck
;;;
(defun restaurant/flycheck-init ()
  (when restaurant/enable-flycheck
    (require 'flycheck)
    (require 'flycheck-pos-tip)
    (flycheck-mode 1)
    (flycheck-pos-tip-mode 1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))))

(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(defun flycheck-print-current-checker (args)
  "Print checker for current buffer.
   ARGS is dummy"
  (interactive "P")
  (print (flycheck-get-checker-for-buffer)))

(add-hook 'prog-mode-hook 'restaurant/flycheck-init)

(defun restaurant/flymake-init ()
  (when restaurant/enable-flymake
    (require 'flymake)
    (flymake-mode 1)))

(add-hook 'prog-mode-hook 'restaurant/flymake-init)
