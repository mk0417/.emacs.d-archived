;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Modus theme is built in latest Emacs master branch
;; Do not need to install
;; (require-package 'modus-themes)

(setq modus-themes-slanted-constructs t
      modus-themes-bold-constructs t
      modus-themes-mode-line 'accented-moody
      modus-themes-syntax 'green-strings
      modus-themes-links 'faint
      modus-themes-fringes 'intense
      modus-themes-prompts 'intense-accented
      modus-themes-completions 'opinionated
      modus-themes-hl-line 'intense-background
      modus-themes-paren-match 'intense-bold
      modus-themes-region 'bg-only)

(setq modus-themes-scale-headings t
      modus-themes-scale-4 1.3
      modus-themes-scale-3 1.2
      modus-themes-scale-2 1.1
      modus-themes-org-blocks 'gray-background)

(setq modus-themes-headings
      '((t . line)))

(setq x-underline-at-descent-line t)

(load-theme 'modus-vivendi t)

;; (modus-themes-load-themes)
;; (modus-themes-load-vivendi)

;; selected text color
(set-face-attribute 'region nil :background "#696969")


(provide 'init-themes)
;;; init-themes.el ends here
