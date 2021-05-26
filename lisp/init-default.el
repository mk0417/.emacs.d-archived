;;; init-default.el --- Better default -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; https://www.masteringemacs.org/article/speed-up-emacs-libjansson-native-elisp-compilation
(if (>= emacs-major-version 28)
    (setq comp-deferred-compilation t
          comp-async-jobs-number 6
          comp-async-report-warnings-errors nil
          package-native-compile t
          load-prefer-newer t))

(setq initial-major-mode 'fundamental-mode)

;; disable bell sound
(setq ring-bell-function 'ignore)

;; highlight current line
(global-hl-line-mode 1)

;; trash
(setq trash-directory "~/.Trash"
      delete-by-moving-to-trash t)

(global-set-key (kbd "C-x k")   'kill-this-buffer)
(global-set-key (kbd "C-x K")   'kill-buffer-and-window)
(global-set-key (kbd "C-w")     'backward-kill-word)


(provide 'init-default)
;;; init-default.el ends here
