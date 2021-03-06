;;; init-minibuffer.el --- Config for minibuffer completion -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'vertico)
  (add-hook 'after-init-hook 'vertico-mode)

  (require-package 'orderless)
  (setq completion-styles '(substring orderless))

  ;; sanityinc/use-orderless-in-minibuffer cause problems on consult--grep
  ;; (with-eval-after-load 'vertico
  ;;   (require 'orderless))
  ;; (defun sanityinc/use-orderless-in-minibuffer ()
  ;;   (setq-local completion-styles '(substring orderless)))
  ;; (add-hook 'minibuffer-setup-hook 'sanityinc/use-orderless-in-minibuffer)

  (when (maybe-require-package 'embark)
    (global-set-key (kbd "C-c C-m") 'embark-act)
    (setq embark-prompter 'embark-completing-read-prompter)
    (setq embark-keymap-prompter-key ",")
    (with-eval-after-load 'vertico
      (define-key vertico-map (kbd "C-c C-o") 'embark-export)
      (define-key vertico-map (kbd "C-o") 'embark-act)))

  (when (maybe-require-package 'consult)
    (setq consult-imenu-config
          '((emacs-lisp-mode :toplevel "Functions"
                             :types ((?f "Functions" font-lock-function-name-face)
                                     (?m "Macros"    font-lock-keyword-face)
                                     (?p "Packages"  font-lock-constant-face)
                                     (?t "Types"     font-lock-type-face)
                                     (?v "Variables" font-lock-variable-name-face)))))


    (defmacro sanityinc/no-consult-preview (&rest cmds)
      `(with-eval-after-load 'consult
         (consult-customize ,@cmds :preview-key (kbd "M-v"))))

    (sanityinc/no-consult-preview
     consult-ripgrep
     consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-file consult--source-project-file consult--source-bookmark)


    (when (maybe-require-package 'projectile)
      (setq-default consult-project-root-function 'projectile-project-root))


    (when (and (executable-find "rg") (maybe-require-package 'affe))
      (defun sanityinc/affe-grep-at-point (&optional dir initial)
        (interactive (list prefix-arg (when-let ((s (symbol-at-point)))
                                        (symbol-name s))))
        (affe-grep dir initial))
      (global-set-key (kbd "M-?") 'sanityinc/affe-grep-at-point)
      (sanityinc/no-consult-preview sanityinc/affe-grep-at-point)
      (with-eval-after-load 'affe (sanityinc/no-consult-preview affe-grep)))


    (global-set-key [remap switch-to-buffer] 'consult-buffer)
    (global-set-key [remap switch-to-buffer-other-window] 'consult-buffer-other-window)
    (global-set-key [remap switch-to-buffer-other-frame] 'consult-buffer-other-frame)
    (global-set-key [remap goto-line] 'consult-goto-line)
    (global-set-key (kbd "C-x l")   'consult-line)


    (when (maybe-require-package 'embark-consult)
      (with-eval-after-load 'embark
        (require 'embark-consult)
        (add-hook 'embark-collect-mode-hook 'embark-consult-preview-minor-mode)))))


(when (maybe-require-package 'marginalia)
  (add-hook 'after-init-hook 'marginalia-mode))


(autoload 'consult--grep "consult")

(defun p-consult-at-point-line (&optional initial)
  (interactive)
  (consult-line (thing-at-point 'symbol)))

(defun p-consult-rg-current-dir (&optional initial)
  (interactive "P")
  (if (equal buffer-file-name nil)
      (consult--grep "Ripgrep current dir" consult-ripgrep-command "/Users/ml/" initial)
    (consult--grep "Ripgrep current dir" consult-ripgrep-command (file-name-directory buffer-file-name) initial)))

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


(define-key minibuffer-mode-map (kbd "C-k")   'delete-backward-char)


(provide 'init-minibuffer)
;;; init-minibuffer.el ends here
