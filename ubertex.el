(defstruct revy-screen location display) ; user@location:0.screen

(defface revy-shown-line-face
  '((((type x w32 mac))
     (:foreground "black" :background "red")))
  "")

(defface revy-invisible-face
  '((((type x w32 mac))
     (:font "DejaVu Sans Mono")
     (:height 90)
     (:background "gray20")))
  "")

(set-face-attribute 'revy-invisible-face nil :height 50)
(set-face-attribute 'revy-invisible-face nil :font "DejaVu Sans Mono")

(set-face-background 'revy-invisible-face "gray30")
(set-face-foreground 'revy-invisible-face "gray50")
(set-face-background 'revy-shown-line-face "firebrick4")
(setq revy-cur (make-overlay 0 10 (current-buffer) t t))
(overlay-put revy-cur 'face 'revy-shown-line-face)
(overlay-put revy-cur 'priority 5000)

(defvar revy-hidden '())
;(make-variable-buffer-local 'revy-hidden)



;(delete-overlay revy-cur)

(global-set-key (kbd "<f6>") 'revy-enter)

(setq line-move-ignore-invisible nil)
(setq revy-current-screen (make-revy-screen :location "localhost" :display ":0"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INITIALIZATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-hide ()
  (interactive)
  (revy-unhide)
  (revy-numerize)
  (revy-insert-blank-comments)
  (save-excursion
    ;; Hide preamble and everything up until the first slide:
    (beginning-of-buffer)
    (search-forward-regexp "\\\\begin{overtex}" nil t)
    (let ((overlay (make-overlay 0 (match-beginning 0) (current-buffer) t nil)))
      ;; (overlay-put overlay 'invisible t)
      (overlay-put overlay 'revy t)
      (overlay-put overlay 'face 'revy-invisible-face)
      (push overlay revy-hidden))

    (beginning-of-buffer)
    (let ((n 1))
      (while (search-forward-regexp "\\\\n{\\([0-9]+\\)}\\|\\\\begin{overtex}\\|\\\\end{overtex}\n\\|\\\\pause\\({}\\)?\\|^\n" nil t)
        (let ((overlay (make-overlay (match-beginning 0) (match-end 0) (current-buffer) t nil)))
          (overlay-put overlay 'revy t)
          (overlay-put overlay 'priority 10000)
          (overlay-put overlay 'face 'revy-invisible-face)

          ;; (when (string= (match-string 0) "\\\\begin{overtex})
          (cond ((string= (match-string 0) "\n")
                 ;; (overlay-put overlay 'invisible t)
                 )
                ((string= (match-string 0) "\\end{overtex}\n")
                 ;; (overlay-put overlay 'invisible t)
                 )
                ((or (string= (match-string 0) "\\begin{overtex}")
                     (string= (match-string 0) "\\pause")
                     (string= (match-string 0) "\\pause{}"))
                 (overlay-put overlay 'revy-slide-number n)
                 (incf n)))
          (overlay-put overlay 'revy-slide-number n)
          (when (string= (match-string 0) "\\begin{overtex}")
            ;; (overlay-put overlay 'before-string "")
            ;; (overlay-put overlay 'invisible t)
            )

          ;; (setf revy-hidden (cons overlay revy-hidden))
          (push overlay revy-hidden)
         ;(put-text-property (match-beginning 0) (match-end 0) 'invisible t)
          )))))

(defun revy-numerize ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let ((n 1))
      (while (search-forward-regexp "\\\\begin{overtex}\\|\\\\pause\\({}\\)?" nil t)
      (insert "\\n{" (int-to-string n) "}")
      (incf n)))))

(defun revy-unnumerize ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (replace-regexp "\\\\n{[0-9]+}" "")))


(defun revy-insert-blank-comments()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (replace-string "\\begin{overtex}\n\\end{overtex}" "\\begin{overtex}\n%blank\n\\end{overtex}")))

(defun revy-unhide ()
  (interactive)
  (revy-unnumerize)
  (remove-overlays (buffer-end -1) (buffer-end 1) 'revy t))










;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NAVIGATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-forward ()
  (interactive)
  (search-forward-regexp "\\\\begin{overtex}\n\\|\\\\pause\\({}\\)?" nil t))

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
      (search-forward-regexp "\\\\end{overtex}\\|\\\\pause\\({}\\)?" nil t)
      (setq end (match-end 0))

    (save-excursion
      (search-backward-regexp "\\\\begin{overtex}" nil t)
      (setq start (match-end 0))
      (when (char-equal (char-after start) ?\n)
        (incf start)))

    (move-overlay revy-cur start end (current-buffer))
    (revy-xpdf-goto-slide (revy-slide-number))
    ))

;; Doesn't work on overlay properties.
;; (defun revy-slide-number ()
;;   (interactive)
;;   (save-excursion
;;     (search-backward-regexp "\\\\begin{overtex}\\|\\\\pause\\({}\\)?")
;;     (let ((overlays (overlays-at (point)))
;;           (continue t)
;;           (overlay nil))
;;       (while (and continue (not (null overlays)))
;;         (setq overlay (pop overlays))
;;         (when (overlay-get overlay 'revy-slide-number)
;;           (setq continue nil)))
;;       (if (null overlay)
;;           (message "There is no slide")
;;         (print overlay)

;;         (print (overlay-get overlay 'revy-slide-number))
;;         (message (int-to-string (overlay-get overlay 'revy-slide-number)))))))
;; ;    (message (int-to-string (overlay-get (car (overlays-at (point))) 'revy-slide-number)))))


;;Works on revy-numerize
(defun revy-slide-number ()
  (interactive)
  (save-excursion
    (search-backward-regexp "\\\\n{\\([0-9]+\\)}" nil t)
    (message (match-string 1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMANDS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defadvice async-shell-command (around hide-async-windows activate)
       (save-window-excursion
          ad-do-it))

(defun revy-shell (command &optional screen)
  (save-window-excursion
    (if (null screen)
        (call-process-shell-command command)
      (call-process-shell-command (concat "ssh "
                                   (revy-screen-location screen)
                                   " -T << EOF \n"
                                   " export DISPLAY="
                                   (revy-screen-display screen)
                                   " ; "
                                   command
                                   " \n EOF &")
                                  nil 0
                           ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; XPDF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-xpdf-open (file)
  (revy-shell (concat "xpdf -fullscreen -remote ubertex " file)
              revy-current-screen))

(defun revy-xpdf-goto-slide (slide)
  (let ((slide (if (numberp slide) (int-to-string slide) slide)))
    (revy-shell (concat "xpdf -remote ubertex -exec \"gotoPage("
                        slide
                        ")\"")
                revy-current-screen)))











;; TOOLS
(defun revy-line ()
  (interactive)
  (move-overlay revy-cur (line-beginning-position) (line-end-position) (current-buffer)))
