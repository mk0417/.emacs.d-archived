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
  "files"
  ("i" (find-file "~/Dropbox/org/idea.org") "research idea")
  ("n" (find-file "~/Dropbox/org/note.org") "notes")
  ("t" (find-file "~/Dropbox/org/todo.org") "todo"))

(defhydra hydra-zoom (:color pink)
  ("i" text-scale-increase "zoom in")
  ("o" text-scale-decrease "zoom out")
  ("r" (text-scale-adjust 0) "zoom in" :exit t))


(provide 'init-hydra)
;;; init-hydra.el ends here
