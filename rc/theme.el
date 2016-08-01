(require 'color-theme)
(color-theme-initialize)
;; (color-theme-restaurant)

;; ;; add it to theme
;; (setq hl-paren-colors
;;       (quote
;;        ("orange1" "gray" "greenyellow" "green1" "springgreen1" "cyan1" "slateblue1" "magenta1" "purple")))

(setq custom-theme-directory (concat restaurant/source-directory "/themes"))

(mkdir restaurant/user-data-directory t)
(add-to-list 'custom-theme-load-path (concat restaurant/user-data-directory "/themes")) ;; TODO: add all themes in this folder

(load-theme 'restaurant t)

;; (setq restaurant-theme-force-faces-for-mode t)

;; (set-foreground-color "gray")
