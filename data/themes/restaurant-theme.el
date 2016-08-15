;;; restaurant-theme.el --- An Emacs port of the Atom Dark theme from Atom.io.
;;
;;
;; Author: Jeremy Whitlock <jwhitlock@apache.org
;; Version: 0.2
;; Keywords: themes atom dark
;; URL: https://github.com/whitlockjc/atom-dark-theme-emacs
;;
;; This file is not part of GNU Emacs.
;;
;; Licenese:
;;
;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary
;;
;; An Emacs port of the Atom Dark theme from Atom.io.
;;
;;; Code

(deftheme restaurant
  "Atom Dark - An Emacs port of the Atom Dark theme from Atom.io.")

(custom-theme-set-variables
 'restaurant
 '(fci-rule-column nil)
 '(fill-column restaurant/max-line-length)
 
 ;; backups
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups")))) ;; hardcode it temporary
 '(delete-old-versions t)
 '(make-backup-files t)

 ;; ecb
 '(ecb-source-path (quote (("~/sources/" "sources")))) ;; hardcode it temporary
 )

;; Testing
(custom-theme-set-faces
 'restaurant

 ;; Basic
 '(button ((t (:inherit (link)))))
 '(cursor ((((background light)) (:background "black")) (((background dark)) (:background "RoyalBlue1"))))
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :foreground "#c5c8c6" :background "#1d1f21" :width normal :weight normal :slant normal :underline nil :overline nil :strike-through nil :box nil :inverse-video nil :stipple nil :inherit nil))))
 '(escape-glyph ((t (:foreground "#FF8000"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((t (:family "Sans Serif"))))
 '(header-line ((t (:foreground "grey90" :background "grey20"))))
 '(highlight ((t (:background "RoyalBlue3"))))
 '(lazy-highlight ((((class color) (min-colors 88) (background light)) (:background "paleturquoise")) (((class color) (min-colors 88) (background dark)) (:background "paleturquoise4")) (((class color) (min-colors 16)) (:background "turquoise3")) (((class color) (min-colors 8)) (:background "turquoise3")) (t (:underline (:color foreground-color :style line)))))
 '(link ((t (:inherit font-lock-keyword-face :underline t))))
 '(link-visited ((default (:inherit (link))) (((class color) (background light)) (:foreground "magenta4")) (((class color) (background dark)) (:foreground "violet"))))
 '(fringe ((t (:background "grey24"))))
 '(match ((((class color) (min-colors 88) (background light)) (:background "yellow1")) (((class color) (min-colors 88) (background dark)) (:background "RoyalBlue3")) (((class color) (min-colors 8) (background light)) (:foreground "black" :background "yellow")) (((class color) (min-colors 8) (background dark)) (:foreground "white" :background "blue")) (((type tty) (class mono)) (:inverse-video t)) (t (:background "gray"))))
 '(minibuffer-prompt ((t (:foreground "#FF8000"))))
 '(next-error ((t (:inherit (region)))))
 '(query-replace ((t (:inherit (isearch)))))
 '(region ((t (:background "RoyalBlue3" :foreground "white"))))
 '(secondary-selection ((t (:background "#262626"))))
 '(shadow ((t (:foreground "#7c7c7c"))))
 '(tooltip ((t (:inherit variable-pitch :background "blue" :foreground "#333"))))
 '(trailing-whitespace ((t (:background "#562d56" :foreground "#FD5FF1"))))
 
 ;; Font-lock
 '(font-lock-builtin-face ((t (:background "gray12" :foreground "#DAD085"))))
 '(font-lock-comment-delimiter-face ((default (:inherit (font-lock-comment-face)))))
 '(font-lock-comment-face ((t (:foreground "#7C7C7C"))))
 '(font-lock-constant-face ((t (:foreground "#99CC99"))))
 '(font-lock-doc-face ((t (:inherit (font-lock-string-face)))))
 '(font-lock-function-name-face ((t (:foreground "#FFD2A7" :background "gray12"))))
 '(font-lock-negation-char-face ((t nil)))
 '(font-lock-keyword-face ((t (:foreground "#96CBFE" :background "gray12"))))
 '(font-lock-preprocessor-face ((t :background "darkblue" :foreground "#8996A8")))
 '(font-lock-regexp-grouping-backslash ((t (:inherit font-lock-string-face))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "red"))))
 '(font-lock-string-face ((t (:foreground "#8AE234"))))
 '(font-lock-type-face ((t (:foreground "#CFCB90"))))
 '(font-lock-variable-name-face ((t (:inherit (default)))))
 '(font-lock-warning-face ((t (:background "yellow" :foreground "black" :weight bold))))

 ;; flymake
 '(flymake-errline ((t (:inherit theme-roller--code-error :background "DarkRed" :foreground "black"))))
 '(flymake-warnline ((t (:inherit theme-roller--code-warning :background "yellow" :foreground "black"))))

 ;; hl-line
 '(hl-line ((t (:background "#130061"))))

 ;; col-highlight
 '(col-highlight ((t (:background "#040017"))))

 ;; ECB
 '(ecb-default-highlight-face ((t (:background "dark slate blue"))))

 ;; mode-line
 '(mode-line ((t (:background "grey10" :foreground "#96CBFE"))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88)) (:box (:line-width 2 :color "#1d1f21" :style released-button))) (t (:inherit (highlight)))))
 '(mode-line-inactive ((default (:inherit (mode-line))) (((class color) (min-colors 88) (background light)) (:background "#7c7c7c" :foreground "grey20" :box (:line-width -1 :color "grey75" :style nil) :weight light)) (((class color) (min-colors 88) (background dark)) (:background "grey30" :foreground "grey80" :box (:line-width -1 :color "grey40" :style nil) :weight light))))

 ;; isearch
 '(isearch ((((class color) (min-colors 88) (background light)) (:foreground "lightskyblue1" :background "magenta3")) (((class color) (min-colors 88) (background dark)) (:foreground "brown4" :background "palevioletred2")) (((class color) (min-colors 16)) (:foreground "cyan1" :background "magenta4")) (((class color) (min-colors 8)) (:foreground "cyan1" :background "magenta4")) (t (:inverse-video t))))
 '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown1")) (((class color) (min-colors 88) (background dark)) (:background "red4")) (((class color) (min-colors 16)) (:background "red")) (((class color) (min-colors 8)) (:background "red")) (((class color grayscale)) (:foreground "grey")) (t (:inverse-video t))))
 
 ;; ido-mode
 '(ido-first-match ((t (:foreground "violet" :weight bold))))
 '(ido-only-match ((t (:foreground "#ff982d" :weight bold))))
 '(ido-subdir ((t (:foreground "#8AE234"))))
 '(ido-virtual ((t (:foreground "#7c7c7c"))))

 ;; diff-hl (https://github.com/dgutov/diff-hl)
 '(diff-hl-change ((t (:foreground "#E9C062" :background "#8b733a"))))
 '(diff-hl-delete ((t (:foreground "#CC6666" :background "#7a3d3d"))))
 '(diff-hl-insert ((t (:foreground "#A8FF60" :background "#547f30"))))

 ;; dired-mode
 '(dired-directory ((t (:inherit (font-lock-keyword-face)))))
 '(dired-flagged ((t (:inherit (diff-hl-delete)))))
 '(dired-symlink ((t (:foreground "#FD5FF1"))))

 ;; guide-key (https://github.com/kai2nenobu/guide-key)
 '(guide-key/highlight-command-face ((t (:inherit (cursor)))))
 '(guide-key/key-face ((t (:inherit (font-lock-warning-face)))))
 '(guide-key/prefix-command-face ((t (:inherit (font-lock-keyword-face)))))

 ;; flx-ido (https://github.com/lewang/flx)
 '(flx-highlight-face ((t (:inherit (link) :weight bold))))

 ;; markdown-mode (http://jblevins.org/projects/markdown-mode/)
 ;;
 ;; Note: Atom Dark Theme for Atom.io does not currently theme some things that markdown-mode does.  For cases where
 ;;       Atom.io does not provide theming, this theme will leave the theming done by markdown-mode as-is.  Where both
 ;;       Atom.io and markdown-mode provide theming, markdown-mode's theming will be changed to match that of Atom.io.
 '(markdown-blockquote-face ((t :foreground "#555")))
 '(markdown-header-face ((t :foreground "#eee")))
 '(markdown-header-delimiter-face ((t (:inherit (markdown-header-face)))))
 '(markdown-header-rule-face ((t (:inherit (font-lock-comment-face)))))

 ;; Js2-mode (https://github.com/mooz/js2-mode)
 '(js2-error ((t (:foreground "#c00"))))
 '(js2-external-variable ((t (:inherit (font-lock-builtin-face)))))
 '(js2-function-param ((t (:foreground "#C6C5FE"))))
 '(js2-jsdoc-html-tag-delimiter ((t (:foreground "#96CBFE"))))
 '(js2-jsdoc-html-tag-name ((t (:foreground "#96CBFE"))))
 '(js2-jsdoc-tag ((t (:inherit (font-lock-doc-face):weight bold))))
 '(js2-jsdoc-type ((t (:inherit (font-lock-type-face)))))
 '(js2-jsdoc-value ((t (:inherit (js2-function-param)))))

 ;; eruby-mode (https://github.com/petere/emacs-eruby-mode)
 '(eruby-standard-face ((t (:inherit (default) :box (:line-width 2 :color "dark slate blue")))))
 '(eruby-standard-face ((t (:inherit (default) :background "gray4" :box (:line-width 2 :color "dark slate blue")))))
 ;; '(eruby-comment-face ((t (:inherit font-lock-comment-face :background "gray20"))))
 ;; '(eruby-standard-face ((t (:background "dark slate blue"))))

 ;; minimap (https://github.com/dengste/minimap)
 '(minimap-active-region-background ((t (:inherit (highlight)))))

 ;; powerline (https://github.com/milkypostman/powerline)
 '(powerline-active2 ((t (:background "grey10"))))

 ;; speedbar
 '(speedbar-button-face ((t (:foreground "#AAAAAA"))))
 '(speedbar-directory-face ((t (:inherit (font-lock-keyword-face)))))
 '(speedbar-file-face ((t (:inherit (default)))))
 '(speedbar-highlight-face ((t (:inherit (highlight)))))
 '(speedbar-selected-face ((t (:background "#4182C4" :foreground "#FFFFFF"))))
 '(speedbar-separator-face ((t (:background "grey11" :foreground "#C5C8C6" :overline "#7C7C7C"))))
 '(speedbar-tag-face ((t (:inherit (font-lock-function-name-face)))))

 ;; whitespace
 '(whitespace-empty ((t (:foreground "#333333"))))
 '(whitespace-hspace ((t (:inherit (whitespace-empty)))))
 '(whitespace-indentation ((t (:inherit (whitespace-empty)))))
 '(whitespace-line ((t (:inherit (trailing-whitespace)))))
 '(whitespace-newline ((t (:inherit (whitespace-empty)))))
 '(whitespace-space ((t (:inherit (whitespace-empty)))))
 '(whitespace-space-after-tab ((t (:inherit (whitespace-empty)))))
 '(whitespace-space-before-tab ((t (:inherit (whitespace-empty)))))
 '(whitespace-tab ((t (:inherit (whitespace-empty)))))
 '(whitespace-trailing ((t (:inherit (trailing-whitespace)))))
 )

(defvar restaurant-theme-force-faces-for-mode t
  "If t, restaurant-theme will use Face Remapping to alter the theme faces for
the current buffer based on its mode in an attempt to mimick the Atom Dark
Theme from Atom.io as best as possible.

The reason this is required is because some modes (html-mode, yaml-mode, ...)
do not provide the necessary faces to do theming without conflicting with other
modes.

Current modes, and their faces, impacted by this variable:

* html-mode: font-lock-variable-name-face
* markdown-mode: default
* yaml-mode: font-lock-variable-name-face
")

;; Many modes in Emacs do not define their own faces and instead use standard Emacs faces when it comes to theming.
;; That being said, to have a real "Atom Dark Theme" for Emacs, we need to work around this so that these themes look
;; as much like "Atom Dark Theme" as possible.  This means using per-buffer faces via "Face Remapping":
;;
;;   http://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Remapping.html
;;
;; Of course, this might be confusing to some when in one mode they see keywords highlighted in one face and in another
;; mode they see a different face.  That being said, you can set the `restaurant-theme-force-faces-for-mode` variable to
;; `nil` to disable this feature.
(defun restaurant-theme-change-faces-for-mode ()
  (interactive)
  (and (eq restaurant-theme-force-faces-for-mode t)
       (cond
        ((member major-mode '(conf-mode conf-javaprop-mode html-mode yaml-mode))
         (face-remap-add-relative 'font-lock-variable-name-face '(:inherit (font-lock-keyword-face))))
        ((eq major-mode 'java-mode)
         (face-remap-add-relative 'font-lock-variable-name-face '(:inherit (js2-function-param))))
        ((eq major-mode 'markdown-mode)
         (face-remap-add-relative 'default '(:foreground "#999")))
        ((member major-mode '(javascript-mode js2-mode))
         (face-remap-add-relative 'font-lock-doc-face '(:inherit (font-lock-comment-face)))))))

(add-hook 'after-change-major-mode-hook 'restaurant-theme-change-faces-for-mode)

;;;###autoload
(and load-file-name
     (boundp 'custom-theme-load-path)
     (add-to-list 'custom-theme-load-path
                  (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'restaurant)

;;; restaurant-theme.el ends here
