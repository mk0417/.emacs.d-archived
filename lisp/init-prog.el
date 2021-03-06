;;; init-prog.el --- Programming/coding -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Jupyter
;; https://github.com/nnicandro/emacs-zmq
;; https://github.com/nnicandro/emacs-zmq/issues/19
;; Do not download zmq module from released version that contains .so file
;; Emacs 28 needs .dylib
;; Answer No when first installation and build it to have .dylib file

(when (maybe-require-package 'jupyter)
  (setq jupyter-eval-use-overlays t)

  (defun p-jupyter-remove-line-overlay ()
    (interactive)
    (evil-open-below 0)
    (kill-whole-line)
    (evil-escape)
    (previous-line))

  (defun p-jupyter-eval-block ()
    (interactive)
    (xah-select-block)
    (let (beg end)
      (setq beg (region-beginning) end (region-end))
      (jupyter-eval-region beg end))))


;; Python
(with-eval-after-load 'python
  (setq python-indent-guess-indent-offset-verbose nil
        python-indent-guess-indent-offset nil)

  (general-create-definer p-python-leader-def
    :prefix ";"
    :states '(normal visual)
    :keymaps 'python-mode-map)
  (p-python-leader-def
    "j"  '(:ignore t :which-key "jupyter")
    "jj" 'jupyter-run-repl
    "jr" 'jupyter-eval-line-or-region
    "jf" 'jupyter-eval-defun
    "je" 'p-jupyter-eval-block
    "jR" 'jupyter-repl-restart-kernel
    "jK" 'jupyter-repl-clear-cells
    "jI" 'jupyter-repl-interrupt-kernel
    "ji" 'jupyter-inspect-at-point
    "jC" 'jupyter-eval-remove-overlays
    "jc" 'p-jupyter-remove-line-overlay
    "jw" 'jupyter-repl-pop-to-buffer))


;; R
(when (maybe-require-package 'ess)
  (with-eval-after-load 'ess
    ;; disable flymake
    (add-hook 'ess-r-mode-hook (lambda () (flymake-mode -1)))
    (setq ess-ask-for-ess-directory nil)
    ;; fix: Error running timer 'ess--idle-timer-function': (wrong-type-argument stringp nil)
    ;; https://github.com/emacs-ess/ESS/issues/1102
    (setq ess-can-eval-in-background nil)))


;; Stata
;; Non-melpa packages
(require 'ess-stata-mode)

;; https://github.com/hieutkt/.doom.d/blob/master/config.el
(setq inferior-STA-start-args ""
      inferior-STA-program (executable-find "stata-mp")
      ;; make stata src work in org
      inferior-STA-program-name "")


(with-eval-after-load 'ess
  (general-create-definer p-ess-leader-def
    :prefix ";"
    :states '(normal visual)
    :keymaps 'ess-mode-map)
  (p-ess-leader-def
    "a"  'ess-cycle-assign
    "e"  '(:ignore t :which-key "eval")
    "ef" 'ess-eval-function
    "el" 'ess-eval-line
    "er" 'ess-eval-region-or-line-and-step))


;; Devdocs
(maybe-require-package 'devdocs-browser)


(provide 'init-prog)
;;; init-prog.el ends here
