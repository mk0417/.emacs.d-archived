;;; init-latex.el --- Latex config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'auctex)
(maybe-require-package 'latex-preview-pane)

(general-create-definer p-latex-leader-def
  :prefix ";"
  :states '(normal visual)
  :keymaps 'LaTeX-mode-map)
(p-latex-leader-def
  "p"   '(:ignore t :which-key "latex preview")
  "pp"  '(latex-preview-pane-mode :which-key "toggle latex preview pane"))


(provide 'init-latex)
;;; init-latex.el ends here
