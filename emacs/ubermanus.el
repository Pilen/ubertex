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
\\usepackage{overtex}

\\begin{document}
\\obeylines

\\begin{overtex}
  % forspil
\\end{overtex}
")


(defun revy-manus-preamble ()
  (interactive)
  (beginning-of-buffer)
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
  (beginning-of-buffer)
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


;; Does not catch the space in "text \pause"
(defun revy-manus-clean ()
  (interactive)
  (beginning-of-buffer)
  (let ((i nil))
    (while (search-forward-regexp "^[[:space:]]*[.,-]" nil t)
      (setq i (read-key-sequence "' '=replace"))
      (when (string= i " ")
        (backward-delete-char 1)
        (move-beginning-of-line nil)))
    (beginning-of-buffer)
    ;; (while (search-forward-regexp "\\([.,-]\\)\\\\pause[[:space:]]*$" nil t)
    ;;   (setq i (read-key-sequence "n=next"))
    ;;   (when (string= i " ")
    ;;     (replace-match "\\pause"))))))
    (while (search-forward-regexp "\\([.,-]\\)[ ]*\\(\\\\pause\\)?[[:space:]]*$" nil t)
      (goto-char (match-end 1))
      (when (string= " " (read-key-sequence "' '=replace"))
        (backward-delete-char 1)
        (move-beginning-of-line nil))))
  (message "done")
  (beginning-of-buffer)
  (replace-regexp "\\\\pause\n\\\\end{overtex}" "\n\\\\end{overtex}")
  (indent-region (point-min) (point-max))
  (end-of-buffer))


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
      (next-line)))
  (insert "\n\\end{overtex}")
  (move-beginning-of-line nil)
  (next-line))

(defun revy-manus-pause ()
  (interactive)
  (insert "\\pause\n"))

(defun revy-manus-break ()
  (interactive)
  (insert "\\begin{overtex}\n  % Blank\n\\end{overtex}\n"))

(defun revy-manus-comment (text)
  (interactive "sComment: ")
  (insert "\\comment{" text "}"))
