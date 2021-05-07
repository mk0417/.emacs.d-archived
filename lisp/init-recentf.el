;;; init-recentf.el --- Settings for tracking recent files -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(add-hook 'after-init-hook 'recentf-mode)
(setq-default
 recentf-max-saved-items 50
 recentf-exclude `("/Applications/Emacs.app/Contents/Resources/lisp/" "/tmp/" "/ssh:" ,(concat package-user-dir "/.*-autoloads\\.el\\'")))

(add-to-list 'recentf-exclude (format "%s/\\.emacs\\.d/elpa.*/.*" (getenv "HOME")))


(provide 'init-recentf)
;;; init-recentf.el ends here
