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
            (define-key revy-manus-mode-map (kbd "<f1>") 'revy-manus-insert-comment)
            (define-key revy-manus-mode-map (kbd "<f2>") 'revy-manus-slide)
            (define-key revy-manus-mode-map (kbd "H-<f2>") 'revy-manus-pause)
            (define-key revy-manus-mode-map (kbd "S-<f2>") 'revy-manus-break)
            revy-manus-mode-map)
  )



(defconst revy-overtex-preamble "\\documentclass[14pt]{beamer}
\\usepackage[danish]{babel}
\\usepackage[utf8]{inputenc}
\\usepackage{overtex}

\\begin{document}
\\obeylines

\\begin{overtex}
  % forspil
\\end{overtex}
")

(defconst revy-overtex-postscript "
\\begin{overtex}
  % slut
\\end{overtex}
\\begin{overtex}
  \\elisp{(revy-end-sketch)}
\\end{overtex}
\\end{document}
")

(defun revy-manus-prepare ()
  ;; TODO: (query-replace-regexp "^%%.*$" "") alternatively (query-replace-regexp "^%%.*\n" "")

  (interactive)
  (let ((melody nil))
    (goto-char (point-min))
    (when (search-forward-regexp "\\\\melody{\\([^}]*\\)}" nil t)
      (setq melody (match-string 1)))

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
        (insert "\n")))

    (goto-char (point-min))
    (search-forward "\\begin{song}")
    (delete-region (point-min) (point))
    (insert revy-overtex-preamble)
    (save-excursion
      ;; (search-forward "\\end{song}")
      (replace-string "\\end{song}" "")
      (insert revy-overtex-postscript)
      )

    (when melody
      (goto-char (point-min))
      (search-forward-regexp "\\\\begin{document}" nil t)
      (beginning-of-line)
      (insert "%% Melody: "
              (replace-regexp-in-string "\n[[:space:]]*%*" "\n%% " melody)
              "\n\n"))

    (indent-region (point-min) (point-max))
    (goto-char (point-min))
    (query-replace-regexp "^\\\\act{[^}]*}\n" "")
    (goto-char (point-min))
    (query-replace-regexp "\\\\act{[^}]*}" "")
    ))


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

(defun revy-manus-insert-comment (text)
  (interactive "sComment: ")
  (insert "\\comment{" text "}"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Clean
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Todo match begins and ends
;; Todo make it posible to undo undo
;; Todo ensure no text is outside slides
;; Todo ask to insert \pause on lines missing them
;; Todo dont clean elisp code/latex macros
;; Todo remove text (not comments) outside slides
(defun revy-manus-clean ()
  (interactive)
  (goto-char (point-min))

  ;; Delete characters from bad sets
  (goto-char (point-min))
  (replace-regexp "–" "-")
  (goto-char (point-min))
  (replace-regexp "’" "'")
  (goto-char (point-min))
  (replace-regexp "…" "...")
  (goto-char (point-min))
  (let ((outside t))
    (while (search-forward-regexp "”" nil t)
      (if outside
          (progn
            (if (y-or-n-p "Replace ” with `` ?")
                (progn (backward-delete-char 1)
                       (insert "``")
                       (setq outside nil))
              (when (y-or-n-p "Replace ” with '' ?")
                (backward-delete-char 1)
                (insert "''")
                (setq outside t))))
        (if (y-or-n-p "Replace ” with '' ?")
            (progn (backward-delete-char 1)
                   (insert "''")
                   (setq outside t))
          (when (y-or-n-p "Replace ” with `` ?")
            (backward-delete-char 1)
            (insert "``")
            (setq outside nil))))))

  ;; change ... into ldots?
  (when (y-or-n-p "Should ... be replaced with \ldots (will ask for each occurrence")
    (goto-char (point-min))
    (while (search-forward-regexp "\\.\\.\\.*" nil t)
      (when (save-match-data (y-or-n-p (format "Replace %s with \ldots?" (match-string 0))))
        (if (save-match-data (looking-at "[[:space:]\n]"))
            (replace-match "\\\\ldots")
          (replace-match "\\\\ldots{}")))))


  ;; Delete an almost invisible char
  (goto-char (point-min))
  (replace-regexp " " "")


  (revy-manus-match-environments)
  (revy-manus-textitparens)
  (revy-manus-split)
  (revy-manus-fix-casing)


  ;; Delete beginning punctuation
  (goto-char (point-min))
  (while (search-forward-regexp "^[[:space:]]*\\([.,:;-]\\)" nil t)
    (when (y-or-n-p (concat "Delete: " (match-string 1)))
      (backward-delete-char 1)
      (move-beginning-of-line nil)))

  ;; Delete trailing punctuation
  ;; TODO, delete stuff like: bla bla, \comment{bla bla}\pause
  (goto-char (point-min))
    ;; (while (search-forward-regexp "\\([.,-]\\)\\\\pause[[:space:]]*$" nil t)
    ;;   (setq i (read-key-sequence "n=next"))
    ;;   (when (string= i " ")
    ;;     (replace-match "\\pause"))))))
  (while (search-forward-regexp "\\([.,:;-]\\)[ ]*\\(\\\\pause\\)?[[:space:]]*$" nil t)
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
    (let ((choice (save-match-data
                    (setq replace (if (y-or-n-p "\\textit parens?")
                                      'textit
                                    (if (y-or-n-p "delete parens?")
                                        'delete
                                      nil))))))
      (case choice
        ('textit (replace-match "\\\\textit{\\1}"))
        ('delete (replace-match "\\1"))))))

;; TODO ignore latex macros
;; \begin{overtex}
;;   \textit{abc}\pause
;;   \textit{Abc}
;; \end{overtex}}
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

(defun revy-manus-match-environments ()
  "Ensure begins and ends are matching"
  (goto-char (point-min))
  (let ((outside t))
    (while (search-forward-regexp "\\\\begin{overtex}\\|\\\\end{overtex}" nil t)
      (if (string= "\\begin{overtex}" (match-string 0))
          (if outside
              (setq outside nil)
            (error "duplicate \\begin"))
        ;; \end{overtex}
        (if outside
            (error "duplicate \\end")
          (setq outside t))))))
