;;; init-evil.el --- Evil-mode/Vim -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; this must be set before loading evil
(setq evil-want-keybinding nil)

(when (maybe-require-package 'evil)
  (evil-mode 1)

  (maybe-require-package 'evil-collection)
  (maybe-require-package 'evil-nerd-commenter)
  (maybe-require-package 'expand-region)
  (maybe-require-package 'general)
  (maybe-require-package 'evil-exchange)
  (maybe-require-package 'undo-fu)

  (when (maybe-require-package 'evil-matchit)
    (with-eval-after-load  'evil
      (setq evilmi-shortcut "m")
      (add-hook 'prog-mode-hook 'turn-on-evil-matchit-mode)))

  (when (maybe-require-package 'evil-surround)
    (with-eval-after-load  'evil
      (global-evil-surround-mode 1)))

  (when (maybe-require-package 'evil-escape)
    (with-eval-after-load  'evil
      (evil-escape-mode 1)
      (setq-default evil-escape-key-sequence "fd")))

  (when (maybe-require-package 'evil-exchange)
    (with-eval-after-load  'evil
      (setq evil-exchange-key (kbd "gox"))
      (evil-exchange-install))))

(with-eval-after-load 'evil
  (evil-set-initial-state 'color-rg-mode 'emacs)

  ;; change cursor color
  ;; https://github.com/hlissner/doom-emacs/issues/1848
  (setq evil-normal-state-cursor '(box "#cf5a65")
        evil-insert-state-cursor '(bar "#cf5a65")
        evil-visual-state-cursor '(hollow "#cf5a65"))

  ;; ex-evil replace
  (defun p-ex-evil-buffer-replace ()
    (interactive)
    (evil-ex (concat "%s/")))

  (defun p-ex-evil-selection-replace ()
    (interactive)
    (evil-ex (concat "'<,'>s/")))

  (setq evil-undo-system 'undo-fu
        evil-ex-interactive-search-highlight 'selected-window
        evil-vsplit-window-right t
        evil-split-window-below t)

  (evil-collection-init)

  (define-key evil-normal-state-map (kbd "C-e")  'evil-end-of-line)
  (define-key evil-normal-state-map (kbd "C-u")  'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-i")  'p-delete-backward-to-tab)
  (define-key evil-normal-state-map (kbd "C-r")  'undo-fu-only-redo)
  (define-key evil-normal-state-map (kbd "gn")   'diff-hl-next-hunk)
  (define-key evil-normal-state-map (kbd "gp")   'diff-hl-previous-hunk)
  (define-key evil-normal-state-map (kbd "gP")   'diff-hl-diff-goto-hunk)
  (define-key evil-normal-state-map (kbd "gl")   'evil-shift-right)
  (define-key evil-normal-state-map (kbd "gh")   'evil-shift-left)
  (define-key evil-normal-state-map (kbd "god")  'p-delete-parens)
  (define-key evil-normal-state-map (kbd "gor")  'p-ex-evil-buffer-replace)
  (define-key evil-normal-state-map (kbd "gok")  'p-surround-parens)
  (define-key evil-normal-state-map (kbd "gof")  'p-surround-brackets)
  (define-key evil-normal-state-map (kbd "goh")  'p-surround-curly)
  (define-key evil-normal-state-map (kbd "goa")  'p-surround-asterisk)
  (define-key evil-normal-state-map (kbd "goi")  'p-surround-slash)
  (define-key evil-normal-state-map (kbd "goe")  'p-surround-equal)
  (define-key evil-normal-state-map (kbd "gcy")  'evilnc-copy-and-comment-lines)
  (define-key evil-normal-state-map (kbd "gcc")  'evilnc-comment-or-uncomment-lines)

  (define-key evil-visual-state-map (kbd "C-e")  'evil-end-of-line)
  (define-key evil-visual-state-map (kbd "v")    'er/expand-region)
  (define-key evil-visual-state-map (kbd "gok")  'p-surround-parens)
  (define-key evil-visual-state-map (kbd "gof")  'p-surround-brackets)
  (define-key evil-visual-state-map (kbd "goh")  'p-surround-curly)
  (define-key evil-visual-state-map (kbd "goa")  'p-surround-asterisk)
  (define-key evil-visual-state-map (kbd "goi")  'p-surround-slash)
  (define-key evil-visual-state-map (kbd "goe")  'p-surround-equal)
  (define-key evil-visual-state-map (kbd "gor")  'p-ex-evil-selection-replace)
  (define-key evil-visual-state-map (kbd "gcy")  'evilnc-copy-and-comment-lines)
  (define-key evil-visual-state-map (kbd "gcc")  'evilnc-comment-or-uncomment-lines)

  (define-key evil-insert-state-map (kbd "C-a")  'evil-beginning-of-line)
  (define-key evil-insert-state-map (kbd "C-e")  'end-of-line)
  (define-key evil-insert-state-map (kbd "C-u")  'p-kill-to-begin-of-line)
  (define-key evil-insert-state-map (kbd "C-i")  'p-delete-backward-to-tab)
  (define-key evil-insert-state-map (kbd "C-;")  'p-insert-spaces)

  (define-key evil-ex-completion-map (kbd "C-w") 'backward-kill-word)

  (define-key evil-inner-text-objects-map "f" 'evil-inner-bracket)
  (define-key evil-inner-text-objects-map "h" 'evil-inner-curly)
  (define-key evil-outer-text-objects-map "f" 'evil-a-bracket)
  (define-key evil-outer-text-objects-map "h" 'evil-a-curly)

  (general-create-definer p-space-leader-def
    :prefix "SPC"
    :states '(normal visual))
  (p-space-leader-def
    "SPC" 'execute-extended-command
    "f"   '(:ignore t :which-key "file")
    "ff"  '(find-file :which-key "find file")
    "fi"  '(p-insert-file-name :which-key "insert file path and name")
    "fr"  '(consult-recent-file :which-key "recent file")
    "fs"  '(save-buffer :which-key "save buffer")
    "fo"  '(find-file-other-window :which-key "open file in another window")
    "fp"  '(p-find-file-in-config :which-key "find config file")
    "b"   '(:ignore t :which-key "buffer")
    "bb"  '(consult-buffer :which-key "consult switch buffer")
    "bd"  '(kill-this-buffer :which-key "kill buffer")
    "bD"  '(kill-buffer-and-window :which-key "kill buffer and window")
    "bi"  '(ibuffer :which-key "ibuffer")
    "br"  '(revert-buffer :which-key "revert buffer")
    "bs"  '((lambda () (interactive) (switch-to-buffer "*scratch*")) :which-key "switch to scratch")
    "`"   '((lambda () (interactive) (switch-to-buffer nil)) :which-key "switch to pervious buffer")
    "d"   '(:ignore t :which-key "dired")
    "dd"  '(dired :which-key "dired directory")
    "dj"  '(dired-jump :which-key "dired jump")
    "g"   '(:ignore t :which-key "git")
    "gg"   '(magit :which-key "magit")
    "gg"   '(magit :which-key "magit")
    "w"   '(:ignore t :which-key "window")
    "1"   '(winum-select-window-1 :which-key "select window 1")
    "2"   '(winum-select-window-2 :which-key "select window 2")
    "3"   '(winum-select-window-3 :which-key "select window 3")
    "4"   '(winum-select-window-4 :which-key "select window 4")
    "5"   '(winum-select-window-5 :which-key "select window 5")
    "wd"  '(delete-window :which-key "delete window")
    "wv"  '(evil-window-vsplit :which-key "split window right")
    "ws"  '(evil-window-split :which-key "split window below")
    "wo"  '(delete-other-windows :which-key "delete other windows")
    "e"   '(:ignore t :which-key "editing")
    "eb"  '(beginning-of-defun :which-key "beginning of function")
    "ec"  '(whitespace-cleanup :which-key "clear whitespace")
    "ee"  '(end-of-defun :which-key "end of function")
    "ef"  '(p-select-function :which-key "select function")
    "ei"  '(evilmi-select-items :which-key "select item")
    "es"  '(p-select-block :which-key "select block")
    "ed"  '(p-insert-date :which-key "insert date")
    "ek"  '(p-insert-uk-date :which-key "insert UK date")
    "et"  '(p-html-region-insert-tag :which-key "insert html tag")
    "p"   '(:ignore t :which-key "project")
    "pp"  '(projectile-switch-project :which-key "switch project")
    "s"   '(:ignore t :which-key "search")
    "ss"  '(consult-line :which-key "consult line")
    "sS"  '(p-consult-at-point-line :which-key "consult at-point line")
    "sp"  '(consult-ripgrep :which-key "consult-rg project")
    "sP"  '(sanityinc/consult-ripgrep-at-point :which-key "consult-rg at-point project")
    "sd"  '(p-consult-rg-current-dir :which-key "consult-rg current dir")
    "sD"  '(p-consult-rg-at-point-current-dir :which-key "consult-rg at-point current dir")
    "si"  '(consult-imenu :which-key "consult imenu")
    "so"  '(consult-outline :which-key "consult outline")
    "sr"  '(rg :which-key "rg search directory")
    "sa"  '(ag :which-key "ag search directory")
    "sg"  '(p-google-search :which-key "search on google")
    "sy"  '(p-youtube-search :which-key "search on youtube")
    "n"   '(:ignore t :which-key "note")
    "na"  '(org-agenda :which-key "org agenda")
    "nc"  '(org-capture :which-key "org capture")
    "nj"  '(org-journal-new-entry :which-key "new journal")
    "t"   '(:ignore t :which-key "toggle")
    "tp"  '(variable-pitch-mode :which-key "pitch font mode")
    "ts"  '(sql-connect :which-key "connect sql")
    "h"   '(:ignore t :which-key "hydra")
    "hf"  '(hydra-quick-files/body :which-key "hydra files")
    "ht"  '(hydra-zoom/body :which-key "hydra text zoom")
    "q"   '(:ignore t :which-key "quit")
    "qq"  '(kill-emacs :which-key "quit emacs"))

  (general-create-definer p-comma-leader-def
    :prefix ","
    :states '(normal visual))
  (p-comma-leader-def
    "s"   '(:ignore t :which-key "color rg")
    "sf"  '(color-rg-search-input-in-current-file :which-key "search input in file")
    "sF"  '(color-rg-search-symbol-in-current-file :which-key "search symbol in file")
    "sd"  '(color-rg-search-input :which-key "search input in directory")
    "sD"  '(color-rg-search-symbol :which-key "search symbol in directory")
    "sp"  '(color-rg-search-input-in-project :which-key "search input in project")
    "sP"  '(color-rg-search-symbol-in-project :which-key "search symbol in project")
    "e"   '(:ignore t :which-key "eval")
    "ee"  '(eval-buffer :which-key "eval buffer")
    "er"  '(eval-region :which-key "eval region")))


(provide 'init-evil)
;;; init-evil.el ends here
