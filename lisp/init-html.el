;;; init-html.el --- Editing HTML -*- lexical-binding: t -*-
;;; Commentary:
;; ERB is configured separately in init-ruby
;;; Code:

(require-package 'tagedit)

(with-eval-after-load 'sgml-mode
  (tagedit-add-paredit-like-keybindings)
  (define-key tagedit-mode-map (kbd "M-?") nil)
  (define-key tagedit-mode-map (kbd "M-s") nil)
  (add-hook 'sgml-mode-hook (lambda () (tagedit-mode 1))))

(add-auto-mode 'html-mode "\\.\\(jsp\\|tmpl\\)\\'")

;; insert html tag
;; https://www.youtube.com/watch?v=aJsVD8nIoHA
(defun p-html-region-insert-tag (begin end tag)
  (interactive "r\nsTag:")
  (goto-char end)
  (insert (concat "</" tag ">"))
  (let* ((real-end (set-marker (make-marker) (point))))
    (goto-char begin)
    (insert (concat "<" tag ">"))
    (goto-char real-end)))


(provide 'init-html)
;;; init-html.el ends here
