;;; init-selectrum.el --- Config for selectrum -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'selectrum)
  (add-hook 'after-init-hook 'selectrum-mode)
  (setq-default selectrum-fix-vertical-window-height t)

  (global-set-key (kbd "C-'") 'selectrum-quick-insert)

  (when (maybe-require-package 'selectrum-prescient)
    (require 'prescient)
    (prescient-persist-mode 1)
    (selectrum-prescient-mode 1)
    (global-set-key [remap execute-extended-command] 'execute-extended-command))

  (when (maybe-require-package 'embark)
    (global-set-key (kbd "C-c C-m") 'embark-act)
    (setq embark-prompter 'embark-completing-read-prompter)
    (setq embark-keymap-prompter-key ",")
    (define-key selectrum-minibuffer-map (kbd "C-c C-o") 'embark-export)
    (define-key selectrum-minibuffer-map (kbd "C-o") 'embark-act))

  (when (maybe-require-package 'consult)
    (setq consult-imenu-config
          '((emacs-lisp-mode :toplevel "Functions"
                             :types ((?f "Functions" font-lock-function-name-face)
                                     (?m "Macros"    font-lock-keyword-face)
                                     (?p "Packages"  font-lock-constant-face)
                                     (?t "Types"     font-lock-type-face)
                                     (?v "Variables" font-lock-variable-name-face)))))

    (when (maybe-require-package 'projectile)
      (setq-default consult-project-root-function 'projectile-project-root))

    (when (executable-find "rg")
      (defun sanityinc/consult-ripgrep-at-point (&optional dir initial)
        (interactive (list prefix-arg (when-let ((s (symbol-at-point)))
                                        (symbol-name s))))
        (consult-ripgrep dir initial)))

    (defun p-consult-at-point-line ()
      (interactive)
      (consult-line (thing-at-point 'symbol)))

    (global-set-key (kbd "C-x l")   'consult-line)
    (global-set-key (kbd "M-?") 'sanityinc/consult-ripgrep-at-point)
    (global-set-key [remap switch-to-buffer] 'consult-buffer)
    (global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
    (global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
    (global-set-key [remap goto-line] 'consult-goto-line)

    (with-eval-after-load 'consult
      (setf (alist-get 'consult-buffer consult-config) `(:preview-key, (kbd "M-v")))
      (setf (alist-get 'consult-recent-file consult-config) `(:preview-key, (kbd "M-v")))
      (dolist (cmd '(consult-ripgrep sanityinc/consult-ripgrep-at-point))
        (add-to-list 'consult-config
                     `(,cmd :preview-key ,(kbd "M-P")))))

    (when (maybe-require-package 'embark-consult)
      (with-eval-after-load 'embark
        (require 'embark-consult)
        (add-hook 'embark-collect-mode-hook 'embark-consult-preview-minor-mode)))))

(when (maybe-require-package 'marginalia)
  (add-hook 'after-init-hook 'marginalia-mode)
  (setq-default marginalia-annotators '(marginalia-annotators-heavy)))

(defun p-consult-rg-current-dir (&optional initial)
  (interactive "P")
  (consult--grep "Ripgrep current dir" consult-ripgrep-command (file-name-directory buffer-file-name) initial))

(defun p-consult-rg-other-dir (&optional initial)
  (interactive "P")
  (consult--grep "Ripgrep current dir" consult-ripgrep-command (read-directory-name "consult-rg directory:") initial))

(defun p-consult-rg-at-point-current-dir ()
  (interactive)
  (consult--grep "Ripgrep current dir" consult-ripgrep-command (file-name-directory buffer-file-name) (thing-at-point 'symbol)))

(defun p-consult-fd-local (&optional dir initial)
  (interactive "P")
  (let ((consult-find-command "fd --color=never --full-path ARG OPTS"))
    (consult-find dir initial)))

(defun p-consult-fd-global (&optional initial)
  (interactive "P")
  (let ((consult-find-command "fd --color=never --full-path ARG OPTS"))
    (consult-find "~/" initial)))


;; Non-melpa packages
(require 'color-rg)


(provide 'init-selectrum)
;;; init-selectrum.el ends here
