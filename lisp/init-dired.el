;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(when (maybe-require-package 'diredfl)
  (with-eval-after-load 'dired
    (diredfl-global-mode)
    (require 'dired-x)))

;; Hook up dired-x global bindings without loading it up-front
(define-key ctl-x-map "\C-j" 'dired-jump)
(define-key ctl-x-4-map "\C-j" 'dired-jump-other-window)

(with-eval-after-load 'dired
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (define-key dired-mode-map (kbd "C-c C-q") 'wdired-change-to-wdired-mode)
  (define-key dired-mode-map (kbd "<C-return>") 'xah-open-in-external-app)

  (general-create-definer p-dired-leader-def
    :prefix ";"
    :states '(normal visual)
    :keymaps 'dired-mode-map)
  (p-dired-leader-def
    "m"  '(dired-mark-files-regexp :which-key "mark file by regex")
    "M"  '(dired-mark-files-containing-regexp :which-key "mark file containing by regex")
    "c"  '(dired-do-copy :which-key "copy file")
    "r"  '(dired-rename-file :which-key "rename file")
    "p"  '(dired-up-directory :which-key "parent directory")
    "n"  '(dired-create-empty-file :which-key "add new file")
    "N"  '(dired-create-directory :which-key "add new directory")
    "i"  '(dired-find-file :which-key "enter directory")))

(when (maybe-require-package 'diff-hl)
  (with-eval-after-load 'dired
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)))


(provide 'init-dired)
;;; init-dired.el ends here
