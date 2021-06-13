;;; init-misc.el --- Miscellaneous config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Misc config - yet to be placed in separate files
(add-auto-mode 'tcl-mode "^Portfile\\'")
(fset 'yes-or-no-p 'y-or-n-p)

(add-hook 'prog-mode-hook 'goto-address-prog-mode)
(setq goto-address-mail-face 'link)

(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
(add-hook 'after-save-hook 'sanityinc/set-mode-for-new-scripts)

(defun sanityinc/set-mode-for-new-scripts ()
  "Invoke `normal-mode' if this file is a script and in `fundamental-mode'."
  (and
   (eq major-mode 'fundamental-mode)
   (>= (buffer-size) 2)
   (save-restriction
     (widen)
     (string= "#!" (buffer-substring (point-min) (+ 2 (point-min)))))
   (normal-mode)))


(when (maybe-require-package 'info-colors)
  (with-eval-after-load 'info
    (add-hook 'Info-selection-hook 'info-colors-fontify-node)))


;; Handle the prompt pattern for the 1password command-line interface
(with-eval-after-load 'comint
  (setq comint-password-prompt-regexp
        (concat
         comint-password-prompt-regexp
         "\\|^Please enter your password for user .*?:\\s *\\'")))


(when (maybe-require-package 'regex-tool)
  (setq-default regex-tool-backend 'perl))

(with-eval-after-load 're-builder
  ;; Support a slightly more idiomatic quit binding in re-builder
  (define-key reb-mode-map (kbd "C-c C-k") 'reb-quit))

(add-auto-mode 'conf-mode "^Procfile\\'")

(maybe-require-package 'esup)

;; Non-melpa packages
;; color-rg
(autoload 'color-rg-search-input-in-current-file "color-rg")
(autoload 'color-rg-search-symbol-in-current-file "color-rg")
(autoload 'color-rg-search-input "color-rg")
(autoload 'color-rg-search-symbol "color-rg")
(autoload 'color-rg-search-input-in-project "color-rg")
(autoload 'color-rg-search-symbol-in-project "color-rg")
(autoload 'color-rg-mode "color-rg")

(setq color-rg-mac-load-path-from-shell nil)

;; uchronia
(autoload 'uchronia-mode "uchronia")
(uchronia-mode 1)
(with-eval-after-load 'uchronia
  (diminish 'uchronia-mode))


(provide 'init-misc)
;;; init-misc.el ends here
