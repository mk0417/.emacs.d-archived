;;; init-xah.el --- Xah Lee -*- lexical-binding: t -*-

;;; Commentary:
;;  copy or modify from Xah Lee
;;; Code:

;; select block between blank lines
;; http://ergoemacs.org/emacs/modernization_mark-word.html
(defun xah-select-block ()
  (interactive)
  (if (region-active-p)
      (re-search-forward "\n[ \t]*\n" nil "move")
    (progn
      (skip-chars-forward " \n\t")
      (when (re-search-backward "\n[ \t]*\n" nil "move")
        (re-search-forward "\n[ \t]*\n"))
      (push-mark (point) t t)
      (re-search-forward "\n[ \t]*\n" nil "move")
      (forward-line -1))))

;; open using external app in dired
;; http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html
(defun xah-open-in-external-app (&optional @fname)
  (interactive)
  (let* (($file-list
          (if @fname
              (progn (list @fname))
            (if (string-equal major-mode "dired-mode")
                (dired-get-marked-files)
              (list (buffer-file-name)))))
         ($do-it-p (if (<= (length $file-list) 5)
                       t
                     (y-or-n-p "Open more than 5 files? "))))
    (when $do-it-p
      (cond
       ((string-equal system-type "windows-nt")
        (mapc
         (lambda ($fpath)
           (w32-shell-execute "open" $fpath))
         $file-list))
       ((string-equal system-type "darwin")
        (mapc
         (lambda ($fpath)
           (shell-command
            (concat "open " (shell-quote-argument $fpath))))
         $file-list))
       ((string-equal system-type "gnu/linux")
        (mapc
         (lambda ($fpath) (let ((process-connection-type nil))
                            (start-process "" nil "xdg-open" $fpath)))
         $file-list))))))

(maybe-require-package 'xah-get-thing)
(maybe-require-package 'xah-replace-pairs)

;; https://github.com/xahlee/xah_emacs_init/blob/master/xah_emacs_html.el
(defun xah-forward-html-end-tag ()
  "Move cursor to the next HTML tag's content."
  (interactive)
  (forward-char 1)
  (search-forward "</")
  (backward-char 2))

(defun xah-backward-html-end-tag ()
  "Move cursor to the previous HTML tag's content."
  (interactive)
  (search-backward "</"))


;; http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html
(defvar xah-brackets nil "string of left/right brackets pairs.")
(setq xah-brackets "()[]{}<>（）［］｛｝⦅⦆〚〛⦃⦄“”‘’‹›«»「」〈〉《》【】〔〕⦗⦘『』〖〗〘〙｢｣⟦⟧⟨⟩⟪⟫⟮⟯⟬⟭⌈⌉⌊⌋⦇⦈⦉⦊❛❜❝❞❨❩❪❫❴❵❬❭❮❯❰❱❲❳〈〉⦑⦒⧼⧽﹙﹚﹛﹜﹝﹞⁽⁾₍₎⦋⦌⦍⦎⦏⦐⁅⁆⸢⸣⸤⸥⟅⟆⦓⦔⦕⦖⸦⸧⸨⸩｟｠⧘⧙⧚⧛⸜⸝⸌⸍⸂⸃⸄⸅⸉⸊᚛᚜༺༻༼༽⏜⏝⎴⎵⏞⏟⏠⏡﹁﹂﹃﹄︹︺︻︼︗︘︿﹀︽︾﹇﹈︷︸")
(defvar xah-left-brackets '("(" "{" "[" "<" "〔" "【" "〖" "〈" "《" "「" "『" "“" "‘" "‹" "«" )
  "List of left bracket chars.")
(progn
  ;; make xah-left-brackets based on xah-brackets
  (setq xah-left-brackets '())
  (dotimes ($x (- (length xah-brackets) 1))
    (when (= (% $x 2) 0)
      (push (char-to-string (elt xah-brackets $x))
            xah-left-brackets)))
  (setq xah-left-brackets (reverse xah-left-brackets)))

(defvar xah-right-brackets '(")" "]" "}" ">" "〕" "】" "〗" "〉" "》" "」" "』" "”" "’" "›" "»")
  "list of right bracket chars.")
(progn
  (setq xah-right-brackets '())
  (dotimes ($x (- (length xah-brackets) 1))
    (when (= (% $x 2) 1)
      (push (char-to-string (elt xah-brackets $x))
            xah-right-brackets)))
  (setq xah-right-brackets (reverse xah-right-brackets)))

(defun xah-backward-left-bracket ()
  "Move cursor to the previous occurrence of left bracket.
The list of brackets to jump to is defined by `xah-left-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
  (interactive)
  (re-search-backward (regexp-opt xah-left-brackets) nil t))

(defun xah-forward-right-bracket ()
  "Move cursor to the next occurrence of right bracket.
The list of brackets to jump to is defined by `xah-right-brackets'.
URL `http://ergoemacs.org/emacs/emacs_navigating_keys_for_brackets.html'
Version 2015-10-01"
  (interactive)
  (re-search-forward (regexp-opt xah-right-brackets) nil t))


;; (require 'xah-get-thing)
;; (require 'xah-replace-pairs)

(autoload 'xah-get-bounds-of-thing "xah-get-thing")
(autoload 'xah-get-bounds-of-thing-or-region "xah-get-thing")

(autoload 'xah-replace-pairs-region "xah-replace-pairs")
(autoload 'xah-replace-regexp-pairs-region "xah-replace-pairs")


(defun xah-html-lines-to-list ()
  "Make the current block of lines into a HTML list.
If `universal-argument' is called first, use ordered list <ol> instead of <ul>.
Example: If your cursor is in the following block of text:
cat
dog
becomes:
<ul>
<li>cat</li>
<li>dog</li>
</ul>"
  (interactive)
  (let ($bds $p1 $p2 $input-str $resultStr )
    (setq $bds (xah-get-bounds-of-thing 'block))
    (setq $p1 (car $bds))
    (setq $p2 (cdr $bds))
    (setq $input-str (buffer-substring-no-properties $p1 $p2))
    (save-excursion
      (setq $resultStr
            (with-temp-buffer
              (insert $input-str)
              (goto-char (point-max))
              (insert "\n")
              (progn
                (goto-char (point-min))
                (while
                    (re-search-forward  "\.html$" nil t)
                  (backward-char 1)
                  (xah-html-any-linkify)))
              (goto-char (point-min))
              (while
                  (not (equal (line-end-position) (point-max)))
                (beginning-of-line)
                (when (looking-at "• ")
                  (delete-char 2))
                (when (looking-at "* ")
                  (delete-char 2))
                (when (looking-at "- ")
                  (delete-char 2))
                (when (looking-at "⓪①②③④⑤⑥⑦⑧⑨⑩")
                  (delete-char 1))
                (while (looking-at " ")
                  (delete-char 1))
                (insert "<li>")
                (end-of-line) (insert "</li>")
                (forward-line 1 ))
              (if current-prefix-arg
                  (progn
                    (goto-char (point-min))
                    (insert "<ol>\n")
                    (goto-char (point-max))
                    (insert "</ol>"))
                (progn
                  (goto-char (point-min))
                  (insert "<ul>\n")
                  (goto-char (point-max))
                  (insert "</ul>")))
              (buffer-string))))
    (delete-region $p1 $p2)
    (insert $resultStr)))

(defun xah-html-lines-to-dl ()
  "Make the current block of lines into a HTML dl list.
e.g.
cat . 4 legs
bird . has wings
becomes
<dl>
<dt>cat</dt><dd>4 legs</dd>
<dt>bird</dt><dd>has wings</dd>
</dl>
First occurence of “. ” in each line is used to separate dt and dd. If none found, it's an error. Note, must have space after the period.
If `universal-argument' is called first, ask user to enter a separater marker for dt and dd.
For example, if the input is
cat → 4 legs
bird → has wings "
  (interactive)
  (let ($bds $p1 $p2 $input-str $resultStr $endpos)
    (setq $bds (xah-get-bounds-of-thing 'block))
    (setq $p1 (car $bds))
    (setq $p2 (cdr $bds))
    (setq $input-str (buffer-substring-no-properties $p1  $p2))
    (if current-prefix-arg
        (progn
          (setq $sep (read-string "separator char between dt dd:" )))
      (setq $sep "\\. +" ))
    (save-excursion
      (setq $resultStr
            (with-temp-buffer
              (insert $input-str)
              (goto-char (point-max))
              (insert "\n")
              (goto-char (point-min))
              (while (not (equal (point) (point-max)))
                (beginning-of-line) (insert "<dt>")
                (setq $endpos (line-end-position))
                (if (re-search-forward $sep $endpos )
                    (progn
                      (delete-region (match-beginning 0) (match-end 0))
                      (insert "</dt>\n<dd>")
                      (end-of-line)
                      (insert "</dd>")
                      (forward-line 1 ))
                  (user-error "cannot find period in line. Try call it with universal-argument.")))
              (goto-char (point-min))
              (insert "<dl>\n")
              (goto-char (point-max))
              (insert "</dl>")
              (buffer-string))))
    (delete-region $p1 $p2)
    (insert $resultStr)))

(defun xah-html-dl-to-table ()
  "Change html dl to table.
Cursor must be inside dl tags.
Currently, assume there are only 2 columns."
  (interactive )
  (let ($p1 $p2)
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (search-backward "<dl>" )
        (setq $p1 (point))
        (search-forward "</dl>")
        (setq $p2 (point))))
    (save-restriction
      (narrow-to-region $p1 $p2)

      (goto-char (point-min))
      (re-search-forward "<dl>")
      (replace-match "<table>" t t )

      (goto-char (point-min))
      (search-forward "</dl>")
      (replace-match "</table>" t t )

      (goto-char (point-min))
      (while (search-forward "<dt>" nil t)
        (replace-match "<tr><td>" t t ))

      (goto-char (point-min))
      (while (search-forward "</dt>" nil t)
        (replace-match "</td>" t t ))

      (goto-char (point-min))
      (while (search-forward "<dd>" nil t)
        (replace-match "<td>" t t ))
      (goto-char (point-max))

      (goto-char (point-min))
      (while (search-forward "</dd>" nil t)
        (replace-match "</td></tr>" t t ))
      (goto-char (point-max)))))

(defun xah-html-table-to-dl ()
  "Change html table to dl.
Cursor must be inside table tags.
 <caption> is removed.
 <th> are also removed.
Currently, assume there are only 2 columns."
  (interactive )
  (let ($p1 $p2)
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (search-backward "<table" )
        (setq $p1 (point))
        (search-forward "</table>")
        (setq $p2 (point))))
    (save-restriction
      (narrow-to-region $p1 $p2)

      (goto-char (point-min))

      (re-search-forward "<table *\\([^>]+?\\)>")
      (replace-match "<dl>" t t )

      (goto-char (point-min))
      (search-forward "</table>")
      (replace-match "</dl>" t t )

      (goto-char (point-min))
      (when
          (search-forward "<caption>" nil t)
        (delete-region (line-beginning-position) (line-end-position))
        (when (looking-at "\n")
          (delete-char 1)))

      (goto-char (point-min))
      (when
          (search-forward "<th>" nil t)
        (delete-region (line-beginning-position) (line-end-position))
        (when (looking-at "\n")
          (delete-char 1)))

      (goto-char (point-min))
      (while (re-search-forward "<tr>\n* *<td>" nil t)
        (replace-match "<dt>" t t ))

      (goto-char (point-min))
      (while (re-search-forward "</td>\n* *<td>" nil t)
        (replace-match "</dt>\n<dd>" t t ))

      (goto-char (point-min))
      (while (re-search-forward "</td>\n* *</tr>" nil t)
        (replace-match "</dd>" t t ))

      (goto-char (point-max)))))

(defun xah-html-table-to-ul ()
  "Change html table to ul
Cursor must be inside table tags.
 <caption> is removed.
 <th> are also removed.
Currently, assume there are only 2 columns.
“ → ” is used to separate columns.
If `universal-argument' is called first, prompt for separator string."
  (interactive )
  (let ($p1 $p2
            ($sep (if current-prefix-arg
                      (read-string "Seperator:" "→") " → ")))
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (search-backward "<table" )
        (setq $p1 (point))
        (search-forward "</table>")
        (setq $p2 (point))))
    (save-restriction
      (narrow-to-region $p1 $p2)

      (goto-char (point-min))
      (when
          (search-forward "<caption>" nil t)
        (delete-region (line-beginning-position) (line-end-position))
        (when (looking-at "\n")
          (delete-char 1)))

      (goto-char (point-min))
      (when
          (search-forward "<th>" nil t)
        (delete-region (line-beginning-position) (line-end-position))
        (when (looking-at "\n")
          (delete-char 1)))

      (goto-char (point-min))
      (re-search-forward "<table *\\([^>]+?\\)>")
      (replace-match "<ul>" t t )

      (goto-char (point-min))
      (search-forward "</table>")
      (replace-match "</ul>" t t )

      (goto-char (point-min))
      (while (search-forward "<tr><td>" nil t)
        (replace-match "<li>" t t ))

      (goto-char (point-min))
      (while (search-forward "</td><td>" nil t)
        (replace-match $sep t t ))

      (goto-char (point-min))
      (while (search-forward "</td></tr>" nil t)
        (replace-match "</li>" t t ))

      (goto-char (point-max)))))

(defun xah-html-ul-to-dl (@begin @end @sep @keep-sep-p)
  "Change html unordered list to definition list.
Cursor must be inside <ul></ul> tags.
else, add empty <dt></dt> in the beginning. @keep-sep-p if true, keep it in result."
  (interactive
   (list
    (if (use-region-p) (region-beginning))
    (if (use-region-p) (region-end))
    (read-string "Seperator:" " → " )
    (not (yes-or-no-p "Remove Seperator:"))))
  (let (($p1 (if @begin @begin (save-excursion (search-forward ">" ) (search-backward "<ul>" ) (point))))
        ($p2 (if @end @end (progn (search-backward "<") (search-forward "</ul>") (point)))))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (goto-char (point-min))
      (search-forward "<ul>") (replace-match "<dl>" t t )
      (goto-char (point-min))
      (search-forward "</ul>") (replace-match "</dl>" t t )
      (goto-char (point-min))
      (while (search-forward "</li>" nil "move") (replace-match "</dd>" t t ))
      (if (or (string-equal @sep "") (eq @sep nil))
          (progn
            (goto-char (point-min))
            (while (search-forward "<li>" nil "move")
              (replace-match "<dt></dt>\n<dd>" t t )))
        (progn
          (goto-char (point-min))
          (while (search-forward "<li>" nil "move")
            (replace-match "<dt>" t t ))
          (goto-char (point-min))
          (while (search-forward @sep nil t)
            (replace-match (if @keep-sep-p (concat @sep "</dt>\n<dd>\n") "</dt>\n<dd>\n" )  t t )
            (search-forward "</dd>" nil "move" ))))))
  (when (fboundp 'xah-upcase-sentence) (xah-upcase-sentence)))

(defun xah-html-dl-to-ul ()
  "Change html dl to ul.
Cursor must be inside dl tags.
If `universal-argument' is called first, prompt for separator string."
  (interactive )
  (let ($p1 $p2
            ($sep (if current-prefix-arg
                      (read-string "Seperator:" " → ") " → ")))
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (search-backward "<dl>" )
        (setq $p1 (point))
        (search-forward "</dl>")
        (setq $p2 (point))))
    (save-restriction
      (narrow-to-region $p1 $p2)
      (goto-char (point-min))
      (search-forward "<dl>")
      (replace-match "<ul>" t t )

      (goto-char (point-min))
      (search-forward "</dl>")
      (replace-match "</ul>" t t )

      (goto-char (point-min))
      (while (search-forward "<dt>" nil t)
        (replace-match "<li>" t t ))

      (goto-char (point-min))
      (while (search-forward "</dd>" nil t)
        (replace-match "</li>" t t ))

      (goto-char (point-min))
      (while (re-search-forward "</dt> *\n*<dd>" nil t)
        (replace-match $sep t t ))
      (goto-char (point-max)))))

(defun xah-html-lines-to-table ()
  "Transform the current text block or selection into a HTML table.
If there's a text selection, use the selection as input.
Otherwise, used current text block delimited by empty lines.
@SEPARATOR is a string used as a delimitor for columns.
For example:
a.b.c
1.2.3
with “.” as separator, becomes
<table class=\"nrm\">
<tr><td>a</td><td>b</td><td>c</td></tr>
<tr><td>1</td><td>2</td><td>3</td></tr>
</table>
URL `http://ergoemacs.org/emacs/elisp_make-html-table.html'"
  (interactive)
  (let ($bds
        $p1 $p2
        ($sep (read-string "String for column separation:" ","))
        ($i 0)
        ($j 0))
    (setq $bds (xah-get-bounds-of-thing-or-region 'block))
    (setq $p1 (car $bds))
    (setq $p2 (cdr $bds))

    (when (equal (length $sep) 0) (user-error "separator cannot be empty."))

    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (let ((case-fold-search nil))

          (goto-char (point-max))
          (insert "\n")

          (goto-char (point-min))
          (while (and
                  (search-forward $sep nil "move")
                  (< $i 2000))
            (replace-match "</td><td>")
            (1+ $i))

          (goto-char (point-min))
          (while (and
                  (search-forward "\n" nil "move")
                  (< $j 2000))
            (replace-match "</td></tr> <tr><td>")
            (1+ $j))

          (goto-char (point-max))
          (beginning-of-line)
          (delete-char 8)

          (goto-char (point-min))
          (insert "<table class=\"nrm\"> <tr><td>")

          (goto-char (point-max))
          (insert "</table>"))))))

(defun xah-html-table-to-lines ()
  "inverse of `xah-html-lines-to-table'."
  (interactive)
  (let ( $p1 $p2)
    (search-backward "<table")
    (setq $p1 (point))
    (search-forward "</table>")
    (setq $p2 (point))
    (xah-replace-regexp-pairs-region
     $p1 $p2
     [["<table \\([^>]+?\\)>" ""]
      ["</table>" ""]
      ["<th>" "🖸"]
      ["</th>" "🖸"]
      ["<td>" "🖸"]
      ["</td>" "🖸"]
      ["<tr>" ""]
      ["</tr>" ""]]
     "FIXEDCASE" "LITERAL")
    (xah-replace-regexp-pairs-region
     $p1 $p2
     [["^🖸" ""]
      ["🖸$" ""]
      ["🖸🖸" "|"]]
     "FIXEDCASE" "LITERAL")))


(defun xah-replace-paren-to-bracket (@begin @end)
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((case-fold-search nil))
    (xah-replace-pairs-region @begin @end
                              '(["(" "["]
                                [")" "]"])
                              'REPORT)))

(defun xah-replace-bracket-to-paren (@begin @end)
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((case-fold-search nil))
    (xah-replace-pairs-region @begin @end
                              '(["[" "("]
                                ["]" ")"])
                              'REPORT)))

(defun xah-replace-true-to-false (@begin @end)
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((case-fold-search nil))
    (xah-replace-pairs-region @begin @end
                              '(["true" "false"])
                              'REPORT)))

(defun xah-replace-false-to-true (@begin @end)
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (let ((case-fold-search nil))
    (xah-replace-pairs-region @begin @end
                              '(["false" "true"])
                              'REPORT)))

;; http://ergoemacs.org/emacs/elisp_change_brackets.html
(defun xah-replace-pairs (@from-chars @to-chars)
  (interactive
   (let (($bracketsList
          '("(paren)"
            "{brace}"
            "[square]"
            "<greater>"
            "`emacs'"
            "`markdown`"
            "~tilde~"
            "=equal="
            "\"ascii quote\""
            "[[double square,2]]"
            "“curly quote”"
            "‘single quote’"
            "‹french angle›"
            "«french double angle»"
            "「corner」"
            "『white corner』"
            "【lenticular】"
            "〖white lenticular〗"
            "〈angle〉"
            "《double angle》"
            "〔tortoise〕"
            "〘white tortoise〙"
            "⦅white paren⦆"
            "〚white square〛"
            "⦃white curly⦄"
            "〈pointing angle〉"
            "⦑ANGLE WITH DOT⦒"
            "⧼CURVED ANGLE⧽"
            "⟦math square⟧"
            "⟨math angle⟩"
            "⟪math DOUBLE ANGLE⟫"
            "⟮math FLATTENED PARENTHESIS⟯"
            "⟬math WHITE TORTOISE SHELL⟭"
            "❛HEAVY SINGLE QUOTATION MARK ORNAMENT❜"
            "❝HEAVY DOUBLE TURNED COMMA QUOTATION MARK ORNAMENT❞"
            "❨MEDIUM LEFT PARENTHESIS ORNAMENT❩"
            "❪MEDIUM FLATTENED LEFT PARENTHESIS ORNAMENT❫"
            "❴MEDIUM LEFT CURLY ORNAMENT❵"
            "❬MEDIUM LEFT-POINTING ANGLE ORNAMENT❭"
            "❮HEAVY LEFT-POINTING ANGLE QUOTATION MARK ORNAMENT❯"
            "❰HEAVY LEFT-POINTING ANGLE ORNAMENT❱"
            "none"
            )))
     (list
      (completing-read "Replace this:" $bracketsList )
      (completing-read "To:" $bracketsList ))))
  (let ( $p1 $p2 )
    (if (use-region-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (if (re-search-backward "\n[ \t]*\n" nil "move")
            (progn (re-search-forward "\n[ \t]*\n")
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (if (re-search-forward "\n[ \t]*\n" nil "move")
            (progn (re-search-backward "\n[ \t]*\n")
                   (setq $p2 (point)))
          (setq $p2 (point)))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (let ( (case-fold-search nil)
               $fromLeft
               $fromRight
               $toLeft
               $toRight)
          (cond
           ((string-match ",2" @from-chars  )
            (progn
              (setq $fromLeft (substring @from-chars 0 2))
              (setq $fromRight (substring @from-chars -2))))
           (t
            (progn
              (setq $fromLeft (substring @from-chars 0 1))
              (setq $fromRight (substring @from-chars -1)))))
          (cond
           ((string-match ",2" @to-chars)
            (progn
              (setq $toLeft (substring @to-chars 0 2))
              (setq $toRight (substring @to-chars -2))))
           ((string-match "none" @to-chars)
            (progn
              (setq $toLeft "")
              (setq $toRight "")))
           (t
            (progn
              (setq $toLeft (substring @to-chars 0 1))
              (setq $toRight (substring @to-chars -1)))))
          (cond
           ((string-match "markdown" @from-chars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "`\\([^`]+?\\)`" nil t)
                (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                (replace-match (concat $toLeft "\\1" $toRight ) "FIXEDCASE" ))))
           ((string-match "tilde" @from-chars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "~\\([^~]+?\\)~" nil t)
                (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                (replace-match (concat $toLeft "\\1" $toRight ) "FIXEDCASE" ))))
           ((string-match "ascii quote" @from-chars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "\"\\([^\"]+?\\)\"" nil t)
                (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                (replace-match (concat $toLeft "\\1" $toRight ) "FIXEDCASE" ))))
           ((string-match "equal" @from-chars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "=\\([^=]+?\\)=" nil t)
                (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                (replace-match (concat $toLeft "\\1" $toRight ) "FIXEDCASE" ))))
           (t (progn
                (progn
                  (goto-char (point-min))
                  (while (search-forward $fromLeft nil t)
                    (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                    (replace-match $toLeft "FIXEDCASE" "LITERAL")))
                (progn
                  (goto-char (point-min))
                  (while (search-forward $fromRight nil t)
                    (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                    (replace-match $toRight "FIXEDCASE" "LITERAL")))))))))))


(provide 'init-xah)
;;; init-xah.el ends here
