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

;; disable bell sound
(setq ring-bell-function 'ignore)

;; highlight current line
(global-hl-line-mode 1)

;; trash
(setq trash-directory "~/.Trash"
      delete-by-moving-to-trash t)

;; https://christiantietze.de/posts/2021/06/emacs-trash-file-macos/
(when (memq window-system '(mac ns))
  (defun system-move-file-to-trash (path)
    "Moves file at PATH to the macOS Trash according to `move-file-to-trash' convention.
Relies on the command-line utility 'trash' to be installed.
Get it from:  <http://hasseg.org/trash/>"
    (shell-command (concat "trash -vF \"" path "\"" "| sed -e 's/^/Trashed: /'")
                   nil ;; Name of output buffer
                   "*Trash Error Buffer*")))


(global-set-key (kbd "C-x k")   'kill-this-buffer)
(global-set-key (kbd "C-x K")   'kill-buffer-and-window)
(global-set-key (kbd "C-w")     'backward-kill-word)


(provide 'init-default)
;;; init-default.el ends here
