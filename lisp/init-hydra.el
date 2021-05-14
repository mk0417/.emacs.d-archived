;;; init-hydra.el --- Hydra -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'hydra)

(defhydra hydra-org-presentation (:color pink :hit nil)
  "
^Start/end presentation^             ^Slide^
^----------------------^-------------^-----^--------------
_s_: start                           _n_: next slide
_e_: end                             _p_: previous slide
  "
  ("s" p-org-presentation-on)
  ("e" p-org-presentation-off :exit t)
  ("n" org-tree-slide-move-next-tree)
  ("p" org-tree-slide-move-previous-tree))

(defhydra hydra-quick-files (:color blue)
  "
^Files^                              ^Folders^
^-----^------------------------------^-------^--------------
_i_: research idea                   _n_: my notes
_t_: todo                            _l_: literature
_s_: scratch note                    _j_: journal
_y_: yankpad                         ^ ^
  "
  ("i" (find-file "~/Dropbox/org/idea.org"))
  ("s" (find-file "~/Dropbox/org/note.org"))
  ("t" (find-file "~/Dropbox/org/todo.org"))
  ("y" (find-file "~/Dropbox/org/yankpad.org"))
  ("j" (dired "~/Dropbox/org/journal"))
  ("n" (dired "~/Dropbox/org/p-notes"))
  ("l" (dired "~/Dropbox/literature")))

(defhydra hydra-zoom (:color pink)
  ("i" text-scale-increase "zoom in")
  ("o" text-scale-decrease "zoom out")
  ("r" (text-scale-adjust 0) "zoom in" :exit t))


(with-eval-after-load 'org
  (general-create-definer p-org-leader-def
    :prefix ";"
    :states '(normal visual))
  (p-org-leader-def
    "p"   '(hydra-org-presentation/body :which-key "hydra presentation")))


(provide 'init-hydra)
;;; init-hydra.el ends here
