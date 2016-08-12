(defun restaurant-version (&optional short)
  (interactive)
  (let ((version "0.1-CURRENT")) ;; autoconf-anchor
    (if short
	(princ version)
      (princ (concat "Restaurant Chef IDE version: " version ".\nBuild with Emacs version:\n" (emacs-version) "\n")))))
