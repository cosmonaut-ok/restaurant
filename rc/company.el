;;;
;;; company-mode
;;;
(require 'company)
(require 'company-quickhelp)


;; (add-hook 'after-init-hook 'global-company-mode)

(add-hook 'prog-mode-hook #'(lambda () (company-mode 1)))

(setq company-idle-delay 0
      company-auto-complete t
      company-minimum-prefix-length 3
      company-dabbrev-downcase nil
      company-dabbrev-ignore-case t
      company-dabbrev-code-ignore-case t)

(define-key company-active-map (kbd "C-n") #'company-select-next)
(define-key company-active-map (kbd "C-p") #'company-select-previous)

(eval-after-load 'company
  (lambda ()
    (setq company-quickhelp-delay 1)
    (define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin)
    (global-set-key (kbd "<backtab>") 'company-complete)
    ))

(defun restaurant/local-push-company-backend (backend)
  "Add BACKEND to a buffer-local version of `company-backends'."
  (set (make-local-variable 'company-backends)
       (append (list backend) company-backends)))
