;;; init-typing.el --- Efficient typing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(with-eval-after-load 'general
  (general-evil-setup t)
  ;; parenthesis
  (defun p-insert-paren ()
    (interactive)
    (insert "()")
    (backward-char 1))
  (general-imap "k"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "k" 'p-insert-paren))
  ;; square brackets
  (defun p-insert-sbracket ()
    (interactive)
    (insert "[]")
    (backward-char 1))
  (general-imap "i"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "i" 'p-insert-sbracket))
  ;; curly brackets
  (defun p-insert-cbracket ()
    (interactive)
    (insert "{}")
    (backward-char 1))
  (general-imap "h"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "h" 'p-insert-cbracket))
  ;; exclamation
  (defun p-insert-exc ()
    (interactive)
    (insert "!"))
  (general-imap "g"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "t" 'p-insert-exc))
  ;; at
  (defun p-insert-at ()
    (interactive)
    (insert "@"))
  (general-imap "q"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "a" 'p-insert-at))
  ;; british pound
  (defun p-insert-pound ()
    (interactive)
    (insert "£"))
  (general-imap "y"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "b" 'p-insert-pound))
  ;; dollar
  (defun p-insert-dollar ()
    (interactive)
    (insert "$"))
  (general-imap "l"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "k" 'p-insert-dollar))
  ;; percentage
  (defun p-insert-percent ()
    (interactive)
    (insert "%"))
  (general-imap "f"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "h" 'p-insert-percent))
  ;; carat
  (defun p-insert-carat ()
    (interactive)
    (insert "^"))
  (general-imap "p"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "w" 'p-insert-carat))
  ;; and
  (defun p-insert-and ()
    (interactive)
    (insert "&"))
  (general-imap "a"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "a" 'p-insert-and))
  ;; asterisk
  (defun p-insert-asterisk ()
    (interactive)
    (insert "*"))
  (general-imap "c"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "j" 'p-insert-asterisk))
  ;; underscore
  (defun p-insert-underscore ()
    (interactive)
    (insert "_"))
  (general-imap "u"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "u" 'p-insert-underscore))
  ;; plus
  (defun p-insert-plus ()
    (interactive)
    (insert "+"))
  (general-imap "j"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "j" 'p-insert-plus))
  ;; pipe
  (defun p-insert-pipe ()
    (interactive)
    (insert "|"))
  (general-imap "s"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "g" 'p-insert-pipe))
  ;; tilde
  (defun p-insert-tilde ()
    (interactive)
    (insert "~"))
  (general-imap "b"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "w" 'p-insert-tilde))
  ;; less than
  (defun p-insert-less ()
    (interactive)
    (insert "<"))
  (general-imap "x"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "y" 'p-insert-less))
  ;; greater than
  (defun p-insert-greater ()
    (interactive)
    (insert ">"))
  (general-imap "d"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "y" 'p-insert-greater))
  ;; question
  (defun p-insert-question ()
    (interactive)
    (insert "?"))
  (general-imap "w"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "w" 'p-insert-question))
  ;; r assign
  (defun p-insert-r-assign ()
    (interactive)
    (insert "<-"))
  (general-imap "e"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "j" 'p-insert-r-assign))
  ;; r connect
  (defun p-insert-r-connect ()
    (interactive)
    (insert "%>%"))
  (general-imap "r"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "j" 'p-insert-r-connect))
  ;; hash
  (defun p-insert-hash ()
    (interactive)
    (insert "#"))
  (general-imap "v"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "v" 'p-insert-hash))
  ;; julia pipe
  (defun p-insert-julia-pipe ()
    (interactive)
    (insert "|>"))
  (general-imap "n"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "j" 'p-insert-julia-pipe))
  ;; julia out
  (defun p-insert-julia-out ()
    (interactive)
    (insert "=>"))
  (general-imap "m"
    (general-key-dispatch 'self-insert-command
      :timeout 0.25
      "j" 'p-insert-julia-out)))


(provide 'init-typing)
;;; init-typing.el.el ends here
