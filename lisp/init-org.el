;;; init-org.el --- Org-mode config -*- lexical-binding: t -*-

;;; Commentary:
;; Among settings for many aspects of `org-mode', this code includes
;; an opinionated setup for the Getting Things Done (GTD) system based
;; around the Org Agenda.  I have an "inbox.org" file with a header
;; including
;;     #+CATEGORY: Inbox
;;     #+FILETAGS: INBOX
;; and then set this file as `org-default-notes-file'.  Captured org
;; items will then go into this file with the file-level tag, and can
;; be refiled to other locations as necessary.
;; Those other locations are generally other org files, which should
;; be added to `org-agenda-files-list' (along with "inbox.org" org).
;; With that done, there's then an agenda view, accessible via the
;; `org-agenda' command, which gives a convenient overview.
;; `org-todo-keywords' is customised here to provide corresponding
;; TODO states, which should make sense to GTD adherents.

;;; Code:

(when *is-a-mac*
  (maybe-require-package 'grab-mac-link))

(maybe-require-package 'org-cliplink)

(when (maybe-require-package 'org-superstar)
  (with-eval-after-load 'org
    (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
    (setq org-superstar-remove-leading-stars t
          org-superstar-headline-bullets-list '("◉" "▷" "○")
          org-superstar-item-bullet-alist
          '((?+ . ?•)
            (?* . ?➤)
            (?- . ?–)))))

(global-set-key (kbd "C-c c")   'org-capture)
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)

(defvar sanityinc/org-global-prefix-map (make-sparse-keymap)
  "A keymap for handy global access to org helpers, particularly clocking.")

(define-key sanityinc/org-global-prefix-map (kbd "j") 'org-clock-goto)
(define-key sanityinc/org-global-prefix-map (kbd "l") 'org-clock-in-last)
(define-key sanityinc/org-global-prefix-map (kbd "i") 'org-clock-in)
(define-key sanityinc/org-global-prefix-map (kbd "o") 'org-clock-out)
(define-key global-map (kbd "C-c o") sanityinc/org-global-prefix-map)


;; Various preferences
(setq org-log-done t
      org-edit-timestamp-down-means-later t
      org-hide-emphasis-markers t
      org-catch-invisible-edits 'show
      org-export-coding-system 'utf-8
      org-fast-tag-selection-single-key 'expert
      org-html-validation-link nil
      org-export-kill-product-buffer-when-displayed t
      org-tags-column 80
      org-edit-src-content-indentation 0)


;; Lots of stuff from http://doc.norang.ca/org-mode.html

;; Re-align tags when window shape changes
(with-eval-after-load 'org-agenda
  (add-hook 'org-agenda-mode-hook
            (lambda () (add-hook 'window-configuration-change-hook 'org-agenda-align-tags nil t))))


(maybe-require-package 'writeroom-mode)

(define-minor-mode prose-mode
  "Set up a buffer for prose editing.
This enables or modifies a number of settings so that the
experience of editing prose is a little more like that of a
typical word processor."
  nil " Prose" nil
  (if prose-mode
      (progn
        (when (fboundp 'writeroom-mode)
          (writeroom-mode 1))
        (setq truncate-lines nil)
        (setq word-wrap t)
        (setq cursor-type 'bar)
        (when (eq major-mode 'org)
          (kill-local-variable 'buffer-face-mode-face))
        (buffer-face-mode 1)
        ;;(delete-selection-mode 1)
        (setq-local blink-cursor-interval 0.6)
        (setq-local show-trailing-whitespace nil)
        (setq-local line-spacing 0.2)
        (setq-local electric-pair-mode nil)
        (ignore-errors (flyspell-mode 1))
        (visual-line-mode 1))
    (kill-local-variable 'truncate-lines)
    (kill-local-variable 'word-wrap)
    (kill-local-variable 'cursor-type)
    (kill-local-variable 'blink-cursor-interval)
    (kill-local-variable 'show-trailing-whitespace)
    (kill-local-variable 'line-spacing)
    (kill-local-variable 'electric-pair-mode)
    (buffer-face-mode -1)
    ;; (delete-selection-mode -1)
    (flyspell-mode -1)
    (visual-line-mode -1)
    (when (fboundp 'writeroom-mode)
      (writeroom-mode 0))))

;;(add-hook 'org-mode-hook 'buffer-face-mode)


(setq org-support-shift-select t)


;;; Refiling
(setq org-refile-use-cache nil)

;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

(with-eval-after-load 'org-agenda
  (add-to-list 'org-agenda-after-show-hook 'org-show-entry))

(advice-add 'org-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

;; Exclude DONE state tasks from refile targets
(defun sanityinc/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets."
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'sanityinc/verify-refile-target)

(defun sanityinc/org-refile-anywhere (&optional goto default-buffer rfloc msg)
  "A version of `org-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-refile goto default-buffer rfloc msg)))

(defun sanityinc/org-agenda-refile-anywhere (&optional goto rfloc no-update)
  "A version of `org-agenda-refile' which allows refiling to any subtree."
  (interactive "P")
  (let ((org-refile-target-verify-function))
    (org-agenda-refile goto rfloc no-update)))

;; Targets start with the file name - allows creating level 1 tasks
;;(setq org-refile-use-outline-path (quote file))
(setq org-refile-use-outline-path t)
(setq org-outline-path-complete-in-steps nil)

;; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes 'confirm)

;;; To-do settings
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
              (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
              (sequence "WAITING(w@/!)" "DELEGATED(e!)" "HOLD(h)" "|" "CANCELLED(c@/!)")))
      org-todo-repeat-to-state "NEXT")

(setq org-todo-keyword-faces
      (quote (("NEXT" :inherit warning)
              ("PROJECT" :inherit font-lock-string-face))))


;;; Agenda views
(setq-default org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))


(let ((active-project-match "-INBOX/PROJECT"))

  (setq org-stuck-projects
        `(,active-project-match ("NEXT")))

  (setq org-agenda-compact-blocks t
        org-agenda-sticky t
        org-agenda-start-on-weekday nil
        org-agenda-span 'week
        org-agenda-include-diary nil
        org-agenda-sorting-strategy
        '((agenda habit-down time-up user-defined-up effort-up category-keep)
          (todo category-up effort-up)
          (tags category-up effort-up)
          (search category-up))
        org-agenda-window-setup 'current-window
        org-agenda-custom-commands
        `(("N" "Notes" tags "NOTE"
           ((org-agenda-overriding-header "Notes")
            (org-tags-match-list-sublevels t)))
          ("g" "GTD"
           ((agenda "" nil)
            (tags "INBOX"
                  ((org-agenda-overriding-header "Inbox")
                   (org-tags-match-list-sublevels nil)))
            (stuck ""
                   ((org-agenda-overriding-header "Stuck Projects")
                    (org-agenda-tags-todo-honor-ignore-options t)
                    (org-tags-match-list-sublevels t)
                    (org-agenda-todo-ignore-scheduled 'future)))
            (tags-todo "-INBOX"
                       ((org-agenda-overriding-header "Next Actions")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("HOLD" "WAITING"))
                                (org-agenda-skip-entry-if 'nottodo '("NEXT")))))
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(todo-state-down effort-up category-keep))))
            (tags-todo ,active-project-match
                       ((org-agenda-overriding-header "Projects")
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-INBOX/-NEXT"
                       ((org-agenda-overriding-header "Orphaned Tasks")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("PROJECT" "HOLD" "WAITING" "DELEGATED"))
                                (org-agenda-skip-subtree-if 'nottododo '("TODO")))))
                        (org-tags-match-list-sublevels t)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "/WAITING"
                       ((org-agenda-overriding-header "Waiting")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "/DELEGATED"
                       ((org-agenda-overriding-header "Delegated")
                        (org-agenda-tags-todo-honor-ignore-options t)
                        (org-agenda-todo-ignore-scheduled 'future)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            (tags-todo "-INBOX"
                       ((org-agenda-overriding-header "On Hold")
                        (org-agenda-skip-function
                         '(lambda ()
                            (or (org-agenda-skip-subtree-if 'todo '("WAITING"))
                                (org-agenda-skip-entry-if 'nottodo '("HOLD")))))
                        (org-tags-match-list-sublevels nil)
                        (org-agenda-sorting-strategy
                         '(category-keep))))
            ;; (tags-todo "-NEXT"
            ;;            ((org-agenda-overriding-header "All other TODOs")
            ;;             (org-match-list-sublevels t)))
            )))))


(add-hook 'org-agenda-mode-hook 'hl-line-mode)

;;; Org clock
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(with-eval-after-load 'org
  (org-clock-persistence-insinuate))
(setq org-clock-persist t)
(setq org-clock-in-resume t)

;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Save state changes in the LOGBOOK drawer
(setq org-log-into-drawer t)
;; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)

;; Show clock sums as hours and minutes, not "n days" etc.
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))


;;; Show the clocked-in task - if any - in the header line
(defun sanityinc/show-org-clock-in-header-line ()
  (setq-default header-line-format '((" " org-mode-line-string " "))))

(defun sanityinc/hide-org-clock-from-header-line ()
  (setq-default header-line-format nil))

(add-hook 'org-clock-in-hook 'sanityinc/show-org-clock-in-header-line)
(add-hook 'org-clock-out-hook 'sanityinc/hide-org-clock-from-header-line)
(add-hook 'org-clock-cancel-hook 'sanityinc/hide-org-clock-from-header-line)

(with-eval-after-load 'org-clock
  (define-key org-clock-mode-line-map [header-line mouse-2] 'org-clock-goto)
  (define-key org-clock-mode-line-map [header-line mouse-1] 'org-clock-menu))


(when (and *is-a-mac* (file-directory-p "/Applications/org-clock-statusbar.app"))
  (add-hook 'org-clock-in-hook
            (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                     (concat "tell application \"org-clock-statusbar\" to clock in \"" org-clock-current-task "\""))))
  (add-hook 'org-clock-out-hook
            (lambda () (call-process "/usr/bin/osascript" nil 0 nil "-e"
                                     "tell application \"org-clock-statusbar\" to clock out"))))


;; TODO: warn about inconsistent items, e.g. TODO inside non-PROJECT
;; TODO: nested projects!


;;; Archiving

(setq org-archive-mark-done nil)
(setq org-archive-location "%s_archive::* Archive")


(maybe-require-package  'org-tree-slide)
(maybe-require-package 'olivetti)
(maybe-require-package 'org-journal)


(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
  (when *is-a-mac*
    (define-key org-mode-map (kbd "M-h") nil)
    (define-key org-mode-map (kbd "C-c g") 'grab-mac-link))

  (setq org-modules nil
        org-directory "~/Dropbox/org"
        org-agenda-files '("~/Dropbox/org/todo.org")
        org-journal-dir "~/Dropbox/org/journal"
        org-journal-date-format "%A, %d %B %Y"
        org-journal-file-format "%Y-%m-%d.org"
        org-journal-file-type 'monthly
        org-journal-enable-agenda-integration t)

  ;; Presentation (org-tree-slide + olivetti)
  (setq org-tree-slide-header nil
        org-tree-slide-slide-in-effect t
        org-tree-slide-heading-emphasis nil
        org-tree-slide-cursor-init t
        org-tree-slide-modeline-display 'outside
        org-tree-slide-skip-done nil
        org-tree-slide-skip-comments t
        org-tree-slide-skip-outline-level 5
        org-tree-slide-activate-message (propertize "Presentation mode ON" 'face 'success)
        org-tree-slide-deactivate-message (propertize "Presentation mode OFF" 'face 'success))

  (setq olivetti-body-width 0.7
        olivetti-minimum-body-width 80
        olivetti-recall-visual-line-mode-entry-state t)

  (setq org-tags-column 0)

  (defun p-org-presentation-on ()
    (interactive)
    (progn
      (org-tree-slide-mode 1)
      (olivetti-mode 1)
      (text-scale-increase 5)
      (fringe-mode '(0 . 0))
      (beacon-mode 0)))
  (defun p-org-presentation-off ()
    (interactive)
    (progn
      (org-tree-slide-mode -1)
      (olivetti-mode -1)
      (text-scale-adjust 0)
      (beacon-mode 1)))

  ;; Capturing
  (global-set-key (kbd "C-c c") 'org-capture)
  (setq org-capture-templates
        `(("t" "todo" entry (file ,(concat org-directory "/todo.org")) ; "" => `org-default-notes-file'
           "* TODO %^{Title}\nSCHEDULED: %^t\n")
          ("n" "note" entry (file ,(concat org-directory "/note.org"))
           "* %U\n")
          ("i" "idea" entry (file ,(concat org-directory "/idea.org"))
           "* %^{Title}\n%U\n")))

  (setq org-confirm-babel-evaluate nil
        org-hide-emphasis-markers t)

  (setq org-fontify-quote-and-verse-blocks t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   `((R . t)
     (ditaa . t)
     (dot . t)
     (emacs-lisp . t)
     (gnuplot . t)
     (latex . t)
     (octave . t)
     (plantuml . t)
     (python . t)
     (screen . nil)
     (,(if (locate-library "ob-sh") 'sh 'shell) . t)
     (sql . t)
     (sqlite . t)
     (jupyter . t)))

  ;; Sorce block template
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("b" . "src shell"))
  (add-to-list 'org-structure-template-alist '("p" . "src elisp"))
  (add-to-list 'org-structure-template-alist '("r" . "src R :session r :eval no-export"))
  (add-to-list 'org-structure-template-alist '("t" . "src jupyter-stata :session stata :eval no-export"))
  (add-to-list 'org-structure-template-alist '("j" . "src jupyter-python :session py :eval no-export"))

  ;; Ignore heading with no_heading tag when exporting
  ;; https://emacs.stackexchange.com/questions/9492/is-it-possible-to-export-content-of-subtrees-without-their-headings/17677
  (defun p-org-export-no-heading (backend)
    (org-map-entries (lambda () (delete-region (point-at-bol) (point-at-eol)))
                     "no_heading"))
  (add-hook 'org-export-before-processing-hook 'p-org-export-no-heading)

  ;; Latex
  (plist-put org-format-latex-options :scale 1.8)

  (general-create-definer p-org-leader-def
    :prefix ";"
    :states '(normal visual)
    :keymaps 'org-mode-map)
  (p-org-leader-def
    "."   '(org-toggle-narrow-to-subtree :which-key "narrow to substree")
    ";"   '(org-toggle-latex-fragment :which-key "latex preview")
    "j"   '(:ignore t :which-key "presentation")
    "jj"  '(p-org-presentation-on :which-key "presentation on")
    "jJ"  '(p-org-presentation-off :which-key "presentation off")
    "jn"  '(org-tree-slide-move-next-tree :which-key "next slide")
    "jp"  '(org-tree-slide-move-previous-tree :which-key "previous slide")
    "t"   '(:ignore t :which-key "table")
    "tk"  '(org-table-move-row-up :which-key "move row up")
    "tj"  '(org-table-move-row-down :which-key "move row down")
    "tl"  '(org-table-move-column-right :which-key "move column right")
    "th"  '(org-table-move-column-left :which-key "move column left")
    "tc"  '(org-table-convert-region :which-key "convert region to table")))


(provide 'init-org)
;;; init-org.el ends here
