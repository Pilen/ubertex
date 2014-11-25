;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;π Manus
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'revy-ubermanus)

(define-minor-mode revy-manus-mode
  "Minor mode for converting tex files to and editing manuscripts.
The mode is simple in that it only defines keyboard shortcuts. It
is therefore not required to be enabled for editing a manus, as
the functions can be called on their own."
  :lighter "manus"
  :keymap (let ((revy-manus-mode-map (make-sparse-keymap)))
            (define-key revy-manus-mode-map (kbd "<f6>") 'revy-manus-slide)
            (define-key revy-manus-mode-map (kbd "H-<f6>") 'revy-manus-pause)
            (define-key revy-manus-mode-map (kbd "S-<f6>") 'revy-manus-break)
            revy-manus-mode-map)
  )



(defconst revy-overtex-preamble "\\documentclass[14pt]{beamer}
\\usepackage[danish]{babel}
\\usepackage[utf8]{inputenc}
\\usepackage{../overtex}

\\begin{document}
\\obeylines

\\begin{overtex}
  % forspil
\\end{overtex}
")


(defun revy-manus-prepare ()
  (interactive)
  (goto-char (point-min))
  ;; Fix mellemrum
  (while (search-forward-regexp "\\\\sings{\\|\\\\scene{" nil t)
    (backward-delete-char 7)
    (insert "%% ")
    (search-forward "}")
    (backward-delete-char 1)
    (when (= 91 (char-after (point)))
      (insert " ")
      (search-forward "]"))
    (when (< (point) (line-end-position))
      (insert "\n"))
    )
  (goto-char (point-min))
  (search-forward "\\begin{song}")
  (delete-region (point-min) (point))
  (insert revy-overtex-preamble)
  (save-excursion
    ;(search-forward "\\end{song}")
    (replace-string "\\end{song}" "")
    (insert "
\\begin{overtex}
  % slut
\\end{overtex}")
    )
  (indent-region (point-min) (point-max))
  )


(defun revy-manus-slide ()
  (interactive)
  (when (= (point) (line-beginning-position))
    (backward-char))
  (exchange-point-and-mark)
  (move-beginning-of-line nil)
  (insert "\\begin{overtex}\n")
  (while (< (point) (mark))
    (move-end-of-line nil)
    (when (< (point) (mark))
      (insert "\\pause")
      (move-beginning-of-line nil)
      (forward-line)))
  (insert "\n\\end{overtex}")
  (move-beginning-of-line nil)
  (forward-line))

(defun revy-manus-pause ()
  (interactive)
  (insert "\\pause\n"))

(defun revy-manus-break ()
  (interactive)
  (insert "\\begin{overtex}\n  % Blank\n\\end{overtex}\n"))

(defun revy-manus-comment (text)
  (interactive "sComment: ")
  (insert "\\comment{" text "}"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Clean
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-manus-clean ()
  (interactive)
  (goto-char (point-min))

  (revy-manus-textitparens)
  (revy-manus-split)
  (revy-manus-fix-casing)

  ;; Delete beginning punctuation
  (goto-char (point-min))
  (while (search-forward-regexp "^[[:space:]]*\\([.,-]\\)" nil t)
    (when (y-or-n-p (concat "Delete: " (match-string 1)))
      (backward-delete-char 1)
      (move-beginning-of-line nil)))

  ;; Delete trailing punctuation
  (goto-char (point-min))
    ;; (while (search-forward-regexp "\\([.,-]\\)\\\\pause[[:space:]]*$" nil t)
    ;;   (setq i (read-key-sequence "n=next"))
    ;;   (when (string= i " ")
    ;;     (replace-match "\\pause"))))))
  (while (search-forward-regexp "\\([.,-]\\)[ ]*\\(\\\\pause\\)?[[:space:]]*$" nil t)
      (goto-char (match-end 1))
      (when (y-or-n-p (concat "Delete: " (match-string 1)))
        (backward-delete-char 1)
        (move-beginning-of-line nil)))

  ;; Delete -
  (goto-char (point-min))
  (while (search-forward-regexp "-" nil t)
    (when (y-or-n-p "Delete: -")
      (replace-match "")))

  ;; Delete double pauses
  (goto-char (point-min))
  (replace-regexp "\\\\pause\n\\\\end{overtex}" "\n\\\\end{overtex}")

  ;; Delete space before \pause
  (goto-char (point-min))
  (replace-regexp "[[:space:]]+\\\\pause" "\\\\pause")

  ;; Delete \pause\pause
  (goto-char (point-min))
  (replace-regexp "\\\\pause\\\\pause" "\\\\pause")

  (indent-region (point-min) (point-max))
  (goto-char (point-max))
  (message "done"))


(defun revy-manus-textitparens ()
  "Turn parens into italic"
  (interactive)
  (goto-char (point-min))
  (while (search-forward-regexp "(\\([^)]*\\))" nil t)
    (if (y-or-n-p "\\textit parens?")
        (replace-match "\\\\textit{\\1}")
      (when (y-or-n-p "delete parens?")
        (replace-match "\\1")))))


(defun revy-manus-fix-casing ()
  "Fix lettercasing.
 most slides should start with a Capital letter and subsequent lines with lowercase"
  (interactive)

  (let ((case-fold-search nil))
    ;; Capitalize
    (goto-char (point-min))
    (while (search-forward-regexp
            (rx "\\begin{overtex}" (* (any whitespace "\n")) lower)
            nil t)
      (when (y-or-n-p "Capitalize?")
        (backward-char)
        (capitalize-word 1)))

    ;; Downcase
    (goto-char (point-min))
    (while (search-forward-regexp
            (rx "\\pause" "\n" (* whitespace) upper)
            nil t)
      (when (y-or-n-p "Downcase?")
        (backward-char)
        (downcase-word 1)))
    ))

(defun revy-manus-split ()
  "Split slides into two"
  (interactive)
  (goto-char (point-min))

  (let ((start nil)
        (end nil)
        (lines nil))
    ;; Split on frames with multiple lines, where a line after line 2 starts with a capital letter.
    (while (search-forward-regexp (rx "\\begin{overtex}" (* (any whitespace "\n"))) nil t)
      (setq start (point))
      (search-forward-regexp "\\\\end{overtex}")
      (setq end (match-beginning 0))
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (setq lines (- (line-number-at-pos end) (line-number-at-pos start)))
        (forward-line 2)
        (when (and (looking-at (rx (* (any whitespace "\n")) upper))
                   (y-or-n-p "Split here?"))
          (insert "\\end{overtex}\n\\begin{overtex}\n")
          )
        (goto-char start)


        ))))
