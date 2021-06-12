;;; init.el --- Load the full configuration -*- lexical-binding: t -*-

;;; Commentary:
;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; Code:
;; Produce backtraces when errors occur: can be helpful to diagnose startup issues
(setq debug-on-error t)

(let ((minver "25.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "26.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024)))
  (setq gc-cons-threshold most-positive-fixnum)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))


;; Inherit variables from .zshrc
;; http://ergoemacs.org/emacs/emacs_env_var_paths.html
(let ((p-path
       '("/Users/ml/anaconda3/bin" ":"
         "/Users/ml/anaconda3/bin/jupyter" ":"
         "/usr/local/bin" ":"
         "/usr/local/sbin" ":"
         "/usr/bin" ":"
         "/bin" ":"
         "/usr/sbin" ":"
         "/sbin" ":"
         "/Applications/Stata/StataMP.app/Contents/MacOS/" ":"
         "/Applications/Stata/StataMP.app/Contents/MacOS/stata" ":"
         "/Library/TeX/texbin" ":"
         "/Users/ml/.emacs.d/bin" ":"
         "/Users/ml/.cargo/bin" ":"
         "/Applications/Emacs.app/Contents/MacOS/bin" ":")))
  (setenv "PATH" (mapconcat 'identity p-path ":") )
  (setq exec-path (append p-path (list "." exec-directory))))


;; Bootstrap config
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages
;; (require 'init-exec-path) ;; Set up $PATH
(require 'init-utils) ;; Must come after init-elpa

;; Load configs for specific features and modes
(require-package 'diminish)
(maybe-require-package 'scratch)
(require-package 'command-log-mode)

(require 'init-frame-hooks)
(require 'init-xterm)
(require 'init-themes)
(require 'init-osx-keys)
(require 'init-gui-frames)
(require 'init-isearch)
(require 'init-grep)
(require 'init-uniquify)
(require 'init-ibuffer)
(require 'init-recentf)
;; (require 'init-selectrum)
(require 'init-minibuffer)
(require 'init-hippie-expand)
(require 'init-default)
(require 'init-snippet)
(require 'init-evil)
(require 'init-dired) ;; Must be after init-evil to make sure general is loaded
(require 'init-company) ;; Must be after init-evil to make sure C-m will not be override by evil
(require 'init-windows)
(require 'init-mmm)
(require 'init-editing-utils)
(require 'init-whitespace)
(require 'init-vc)
(require 'init-git)
(require 'init-github)
(require 'init-projectile)
;; I perfer to karabiner
;; (require 'init-typing)
(require 'init-compile)
(require 'init-markdown)
(require 'init-csv)
(require 'init-javascript)
(require 'init-nxml)
(require 'init-html)
(require 'init-css)
(require 'init-http)
(require 'init-sql)
(require 'init-toml)
(require 'init-yaml)
(require 'init-paredit)
(require 'init-lisp)
(require 'init-slime)
(require 'init-common-lisp)
;; load init-prog before init-org
;; otherwise cannot find ob-jupyter when load init-org
(require 'init-prog)
(require 'init-org)
(require 'init-hydra)

(when *spell-check-support-enabled*
  (require 'init-spelling))

(require 'init-misc)


;; Extra packages which don't require any configuration
(require-package 'sudo-edit)
(require-package 'gnuplot)
(require-package 'lua-mode)
(require-package 'htmlize)
(when *is-a-mac*
  (require-package 'osx-location))
(maybe-require-package 'dotenv-mode)
(maybe-require-package 'shfmt)

(when (maybe-require-package 'uptimes)
  (setq-default uptimes-keep-count 200)
  (add-hook 'after-init-hook (lambda () (require 'uptimes))))

(when (fboundp 'global-eldoc-mode)
  (add-hook 'after-init-hook 'global-eldoc-mode)
  (diminish 'eldoc-mode))

;; Allow access from emacsclient
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

;; Variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

;; Locales (setting them earlier in this file doesn't work in X)
(require 'init-locales)


(provide 'init)
;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
