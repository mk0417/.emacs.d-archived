;;; init-themes.el --- Defaults for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'modus-themes)

(setq modus-themes-syntax 'green-strings
      modus-themes-links 'faint
      modus-themes-fringes 'intense
      modus-themes-prompts 'intense-accented
      modus-themes-completions 'opinionated
      modus-themes-hl-line 'intense-background
      modus-themes-paren-match 'intense-bold
      modus-themes-region 'bg-only)

(setq modus-themes-scale-headings t
      modus-themes-scale-1 1.5
      modus-themes-scale-2 1.3
      modus-themes-scale-2 0.9
      modus-themes-org-blocks 'gray-background)

(setq modus-themes-headings
      '((t . line)))

(load-theme 'modus-vivendi t)

;; selected text color
(set-face-attribute 'region nil :background "#696969")


(provide 'init-themes)
;;; init-themes.el ends here
