;;; init-evil.el --- Evil-mode/Vim -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; this must be set before loading evil
(setq evil-collection-want-unimpaired-p nil)
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
      (setq evil-escape-excluded-states '(normal multiedit emacs motion))
      ;; Disable fd in minibuffer
      ;; https://github.com/hlissner/doom-emacs/blob/develop/modules/editor/evil/config.el
      (add-hook 'evil-escape-inhibit-functions
                (defun p-evil-inhibit-escape-in-minibuffer ()
                  (and (minibufferp)
                       (or (not (bound-and-true-p evil-collection-setup-minibuffer))
                           (evil-normal-state-p)))))
      (evil-escape-mode 1)
      (setq-default evil-escape-key-sequence "fd"))
    (diminish 'evil-escape-mode))

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
  (define-key evil-normal-state-map (kbd "C-b")  'backward-char)
  (define-key evil-normal-state-map (kbd "gn")   'diff-hl-next-hunk)
  (define-key evil-normal-state-map (kbd "gp")   'diff-hl-previous-hunk)
  (define-key evil-normal-state-map (kbd "gP")   'diff-hl-diff-goto-hunk)
  (define-key evil-normal-state-map (kbd "gl")   'evil-shift-right)
  (define-key evil-normal-state-map (kbd "gh")   'evil-shift-left)
  (define-key evil-normal-state-map (kbd "tp")   'p-replace-paren-to-bracket)
  (define-key evil-normal-state-map (kbd "tP")   'p-replace-bracket-to-paren)
  (define-key evil-normal-state-map (kbd "tt")   'p-replace-true-to-false)
  (define-key evil-normal-state-map (kbd "tT")   'p-replace-false-to-true)
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
  (define-key evil-visual-state-map (kbd "C-b")  'backward-char)
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
  (define-key evil-insert-state-map (kbd "C-k")  'delete-backward-char)

  (define-key evil-ex-completion-map (kbd "C-w") 'backward-kill-word)
  (define-key evil-ex-completion-map (kbd "C-f") 'forward-char)
  (define-key evil-ex-completion-map (kbd "C-b") 'backward-char)
  (define-key evil-ex-completion-map (kbd "C-k") 'delete-backward-char)

  (define-key evil-inner-text-objects-map "f" 'evil-inner-bracket)
  (define-key evil-inner-text-objects-map "h" 'evil-inner-curly)
  (define-key evil-inner-text-objects-map "d" 'evil-inner-double-quote)
  (define-key evil-outer-text-objects-map "f" 'evil-a-bracket)
  (define-key evil-outer-text-objects-map "h" 'evil-a-curly)
  (define-key evil-outer-text-objects-map "d" 'evil-a-double-quote)

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
    "fR"  '(rename-this-file-and-buffer :which-key "rename file")
    "b"   '(:ignore t :which-key "buffer")
    "bb"  '(consult-buffer :which-key "consult switch buffer")
    "bd"  '(kill-this-buffer :which-key "kill buffer")
    "bD"  '(kill-buffer-and-window :which-key "kill buffer and window")
    "bi"  '(ibuffer :which-key "ibuffer")
    "bo"  '(consult-buffer-other-window :which-key "open file in another window")
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
    "ea"  '(beginning-of-defun :which-key "beginning of function")
    "ec"  '(whitespace-cleanup :which-key "clear whitespace")
    "ee"  '(end-of-defun :which-key "end of function")
    "ef"  '(p-select-function :which-key "select function")
    "ei"  '(evilmi-select-items :which-key "select item")
    "es"  '(p-select-block :which-key "select block")
    "ed"  '(p-insert-date :which-key "insert date")
    "ek"  '(p-insert-uk-date :which-key "insert UK date")
    "ey"  '(yankpad-insert :which-key "insert yankpad snippet")
    "ep"  '(p-replace-pairs :which-key "replace pairs")
    "p"   '(:ignore t :which-key "project")
    "pp"  '(projectile-switch-project :which-key "switch project")
    "s"   '(:ignore t :which-key "search")
    "ss"  '(consult-line :which-key "consult line")
    "sS"  '(p-consult-at-point-line :which-key "consult at-point line")
    "sm"  '(consult-multi-occur :which-key "consult multi occur")
    "sp"  '(consult-ripgrep :which-key "consult-rg project")
    "sP"  '(sanityinc/affe-grep-at-point :which-key "consult-rg at-point project")
    "sd"  '(p-consult-rg-current-dir :which-key "consult-rg current dir")
    "sD"  '(p-consult-rg-at-point-current-dir :which-key "consult-rg at-point current dir")
    "so"  '(p-consult-rg-other-dir :which-key "consult-rg other dir")
    "sf"  '(p-consult-fd-global :which-key "consult-fd global files")
    "sF"  '(p-consult-fd-local :which-key "consult-fd local files")
    "si"  '(consult-imenu :which-key "consult imenu")
    "sl"  '(consult-outline :which-key "consult outline")
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
    "tw"  '(count-words :which-key "count words")
    "tl"  '(count-lines-page :which-key "count lines")
    "tf"  '(p-display-file-name :which-key "show file name")
    "h"   '(:ignore t :which-key "hydra")
    "hf"  '(hydra-quick-files/body :which-key "hydra files")
    "ht"  '(hydra-zoom/body :which-key "hydra text zoom")
    "hh"  '(hydra-html-util/body :which-key "hydra html util")
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
    "ef"  '(eval-buffer :which-key "eval buffer")
    "er"  '(eval-region :which-key "eval region")
    "ee"  '(eval-expression :which-key "eval expression")
    "el"  '(eval-last-sexp :which-key "eval last sexp")
    "h"   '(:ignore t :which-key "uchronia")
    "hr"  '(uchronia-repeat :which-key "uchronia repeat")
    "hs"  '(uchronia-select :which-key "uchronia select")
    "d"   '(:ignore t :which-key "devdocs")
    "di"  '(devdocs-browser-install-docs :which-key "devdocs install docs")
    "dI"  '(devdocs-browser-uninstall-docs :which-key "devdocs uninstall docs")
    "du"  '(devdocs-browser-update-docs :which-key "devdocs update docs")
    "dU"  '(devdocs-browser-upgrade-docs :which-key "devdocs upgrade docs")
    "do"  '(devdocs-browser-open-in :which-key "devdocs open in"))

  (general-create-definer p-text-leader-def
    :prefix ";"
    :states '(normal visual))
  (p-text-leader-def
    "c"   '(:ignore t :which-key "spell check")
    "cc"  '(flyspell-buffer :which-key "spell buffer")
    "cw"  '(flyspell-word :which-key "spell word")
    "cn"  '(flyspell-goto-next-error :which-key "next spell error"))

  (general-create-definer p-html-leader-def
    :prefix ";"
    :states '(normal visual)
    :keymaps '(html-mode-map org-mode-map markdown-mode-map))
  (p-html-leader-def
    "g"  '(:ignore t :which-key "html tag")
    "gi"  '(p-html-region-insert-tag :which-key "insert html tag")
    "gf"  '(xah-forward-html-end-tag :which-key "forward html end tag")
    "gb"  '(xah-backward-html-end-tag :which-key "backward html end tag")
    "l"  '(:ignore t :which-key "html lines")
    "ll" '(xah-html-lines-to-list :which-key "lines to list")
    "ld" '(xah-html-lines-to-dl :which-key "lines to dl")
    "lt" '(xah-html-lines-to-table :which-key "lines to table")
    "d"  '(:ignore t :which-key "html dl")
    "du" '(xah-html-dl-to-ul :which-key "dl to ul")
    "dt" '(xah-html-dl-to-table :which-key "dl to table")
    "dd" '(xah-html-ul-to-dl :which-key "ul to dl")
    "t"  '(:ignore t :which-key "html table")
    "td" '(xah-html-table-to-dl :which-key "table to dl")
    "tu" '(xah-html-table-to-ul :which-key "table to ul")
    "tl" '(xah-html-table-to-lines :which-key "table to lines")))


(provide 'init-evil)
;;; init-evil.el ends here
