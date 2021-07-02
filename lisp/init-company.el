;;; init-company.el --- Completion with company -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(setq company-idle-delay 0
      company-minimum-prefix-length 1)

(when (maybe-require-package 'company)
  (add-hook 'after-init-hook 'global-company-mode)
  (with-eval-after-load 'company
    (setq company-backends
          '(company-files
            company-capf
            company-keywords
            company-dabbrev
            company-dabbrev-code
            company-abbrev))
    (custom-set-faces
     '(company-preview-common
       ((t (:inherit company-preview))))
     '(company-tooltip
       ((t (:background "#ffeead" :foreground "black"))))
     '(company-tooltip-selection
       ((t (:background "#69adc6" :foreground "white"))))
     '(company-tooltip-annotation
       ((t (:background "#ffeead" :foreground "red"))))
     '(company-tooltip-common
       ((t (:background "#ffeead" :foreground "black")))))
    (diminish 'company-mode)
    (define-key company-mode-map (kbd "C-j") 'company-complete)
    (define-key company-active-map (kbd "C-m") 'company-complete-selection)
    (define-key company-active-map (kbd "C-w") 'backward-kill-word)
    (define-key company-active-map (kbd "C-k") 'delete-backward-char)
    (define-key company-mode-map [remap completion-at-point] 'company-complete)
    (define-key company-mode-map [remap indent-for-tab-command] 'company-indent-or-complete-common)
    (define-key company-active-map (kbd "M-/") 'company-other-backend)
    (define-key company-active-map (kbd "C-n") 'company-select-next)
    (define-key company-active-map (kbd "C-p") 'company-select-previous)
    (define-key company-active-map (kbd "C-d") 'company-show-doc-buffer)
    (define-key company-active-map (kbd "M-.") 'company-show-location)
    (setq-default company-dabbrev-other-buffers 'all
                  company-tooltip-align-annotations t))
  (global-set-key (kbd "M-C-/") 'company-complete))


;; citre
(when (maybe-require-package 'citre)
  (add-hook 'find-file-hook #'citre-auto-enable-citre-mode)
  (setq citre-readtags-program "/usr/local/bin/readtags"
        citre-project-root-function #'projectile-project-root))

(defun p-citre-create-tag (lang)
  (interactive "sLanguage:")
  (shell-command (format "ctags --languages=%s --kinds-all='*' --fields='*' --extras='*' -R" lang)))


;; Company-box does not work with Emacs with no titlebar
;; (when (maybe-require-package 'company-box)
;;   (add-hook 'company-mode-hook 'company-box-mode)
;;   (setq company-box-doc-enable nil
;;         company-box-backends-colors nil)
;;   (with-eval-after-load 'company-box
;;     (diminish 'company-box-mode)))


(provide 'init-company)
;;; init-company.el ends here
