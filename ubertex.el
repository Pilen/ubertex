(defstruct revy-screen user location screen) ; user@location:0.screen

(defface revy-shown-line-face
  '((((type x w32 mac))
     (:foreground "black" :background "red")))
  "")

(defface revy-invisible-face
  '((((type x w32 mac))
     (:background "gray20")))
  "")

(set-face-background 'revy-invisible-face "gray30")
(set-face-foreground 'revy-invisible-face "gray50")
(set-face-background 'revy-shown-line-face "firebrick4")
(setq revy-cur (make-overlay 0 10 (current-buffer) t t))
(overlay-put revy-cur 'face 'revy-shown-line-face)
(overlay-put revy-cur 'priority 5000)

(defvar revy-hidden '())
;(make-variable-buffer-local 'revy-hidden)



;(delete-overlay revy-cur)


;; INITIALIZATION
(defun revy-hide ()
  (interactive)
  (revy-insert-blank-comments)
  (save-excursion
    ;; Hide preamble and everything up until the first slide:
    (beginning-of-buffer)
    (search-forward-regexp "\\\\begin{overtex}")
    (let ((overlay (make-overlay 0 (match-beginning 0) (current-buffer) t nil)))
      (overlay-put overlay 'invisible t)
      (overlay-put overlay 'revy t)
      (overlay-put overlay 'face 'revy-invisible-face)
      (push overlay revy-hidden))

    (beginning-of-buffer)
    (let ((n 1))
      (while (search-forward-regexp "\\\\begin{overtex}\\|\\\\end{overtex}\n\\|\\\\pause\\({}\\)?\\|^\n" nil t)
        (let ((overlay (make-overlay (match-beginning 0) (match-end 0) (current-buffer) t nil)))
          (overlay-put overlay 'revy t)
          (overlay-put overlay 'priority 10000)
          (overlay-put overlay 'face 'revy-invisible-face)

          (cond ((string= (match-string 0) "\n")
                 (overlay-put overlay 'invisible t))
                ((string= (match-string 0) "\\end{overtex}\n")
                 (overlay-put overlay 'invisible t))
                (t
                 (overlay-put overlay 'revy-slide-number n)
                 (incf n)))
          (when (string= (match-string 0) "\\begin{overtex}")
            ;; (overlay-put overlay 'before-string "")
            (overlay-put overlay 'invisible t))

          ;; (setf revy-hidden (cons overlay revy-hidden))
          (push overlay revy-hidden)
         ;(put-text-property (match-beginning 0) (match-end 0) 'invisible t)
          )))))

(defun revy-insert-blank-comments()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (replace-string "\\begin{overtex}\n\\end{overtex}" "\\begin{overtex}\n%blank\n\\end{overtex}")))


(defun revy-unhide ()
  (interactive)
  (remove-overlays (buffer-end -1) (buffer-end 1) 'revy t))











;; NAVIGATION
(defun revy-forward ()
  (interactive)
  (search-forward-regexp "\\\\begin{overtex}\n\\|\\\\pause\\({}\\)?"))

(defun revy-next ()
  (interactive)
  (let ((start (point)))
    (goto-char (overlay-end revy-cur))
    ;(search-forward-regexp "\\\\begin{overtex}\\|\\\\end{overtex}\\|\\\\pause\\({}\\)?")
    (revy-mark)
    (goto-char (overlay-end revy-cur))
    ;; (when (string= (match-string 0) "\\end{overtex}")
    ;;   (message "kat"))
    ;;   (revy-mark)
      ))

(defun revy-enter ()
  (interactive)
  (let ((start nil)
        (end nil))

    ;(save-excursion
      (search-forward-regexp "\\\\end{overtex}\\|\\\\pause\\({}\\)?")
      (setq end (match-end 0))

    (save-excursion
      (search-backward-regexp "\\\\begin{overtex}")
      (setq start (match-end 0))
      (when (char-equal (char-after start) ?\n)
        (incf start)))

    (move-overlay revy-cur start end (current-buffer))
    (revy-slide-number)
    ))

(defun revy-slide-number ()
  (interactive)
  (save-excursion
    (search-backward-regexp "\\\\begin{overtex}\\|\\\\pause\\({}\\)?")
    (let ((overlays (overlays-at (point)))
          (continue t)
          (overlay nil))
      (while (and continue (not (null overlays)))
        (setq overlay (pop overlays))
        (when (overlay-get overlay 'revy-slide-number)
          (setq continue nil)))
      (if (null overlay)
          (message "There is no slide")
        (message (int-to-string (overlay-get overlay 'revy-slide-number)))))))
;    (message (int-to-string (overlay-get (car (overlays-at (point))) 'revy-slide-number)))))

(global-set-key (kbd "<f6>") 'revy-enter)






(setq line-move-ignore-invisible nil)

















;; TOOLS
(defun revy-line ()
  (interactive)
  (move-overlay revy-cur (line-beginning-position) (line-end-position) (current-buffer)))

(defun revy-numerize ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let ((n 1))
      (while (search-forward-regexp "\\\\begin{overtex}\\|\\\\pause\\({}\\)?")
      (insert "[" (int-to-string n) "]")
      (incf n)))))

(defun revy-unnumerize ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (while (search-forward-regexp "\\\\begin{overtex}\\[[0-9]+\\]\\|\\\\pause\[[0-9]+\\]")
      (search-backward-regexp "\\[[0-9]+\\]")
      (delete-region (match-beginning 0) (match-end 0)))))
