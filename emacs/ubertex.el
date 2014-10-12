;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;π Ubertex
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'revy-ubertex)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defvar revy-ubertex-hidden '())
;(make-variable-buffer-local 'revy-ubertex-hidden)

;(delete-overlay revy-cursor)

;;;;; (setq line-move-ignore-invisible nil) Set this?


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Minor mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode revy-ubertex-mode latex-mode "Ubertex"
  "Major mode for ubertex buffers
The standard entry for opening an overtex file and playing it's sketch.
Starts the sketch from the beginning.
Based off `latex-mode' so it will work with both the standard latex mode and AUCTeX."
  ;; :keymap (let ((revy-ubertex-mode-map (make-sparse-keymap)))
  ;;           (define-key revy-ubertex-mode-map (kbd "<next>") 'revy-ubertex-next)
  ;;           (define-key revy-ubertex-mode-map (kbd "<home>") 'revy-ubertex-start)
  ;;           (define-key revy-ubertex-mode-map (kbd "<end>") 'revy-ubertex-enter)
  ;;           (define-key revy-ubertex-mode-map (kbd "<delete>") 'revy-blank)
  ;;           (define-key revy-ubertex-mode-map (kbd "<f11>") 'revy-ubertex-hide)
  ;;           (define-key revy-ubertex-mode-map (kbd "<f12>") 'revy-unhide)
  ;;           revy-ubertex-mode-map)

  ;; Go to beginning
  (goto-char (point-min))

  ;; Setup metafunctions
  (setq revy-mode-enter-function 'revy-ubertex-enter)
  (setq revy-mode-forward-function 'revy-forward-enter)

  ;; Clear old cursors
  (revy-unhide) ;; remove revy-unhide entirely
  (revy-clear-overlays)

  ;; Create new local cursor
  (setq revy-local-cursor (make-overlay 0 0 (current-buffer) t t))
  (overlay-put revy-local-cursor 'face 'revy-local-cursor-face)
  (overlay-put revy-local-cursor 'priority 4999)
  (overlay-put revy-local-cursor 'revy t)

  ;; Prepare buffer.
  (revy-ubertex-hide)

  ;; Transfer pdf
  (revy-sync-files)
  (let ((filename (concat (file-name-sans-extension (buffer-file-name)) ".pdf")))
    (when revy-scp-mode
      (revy-scp-file filename "pdfs")))

  ;; Open pdf on worker
  (revy-pdf-open (revy-data-path (buffer-file-name) ".pdf")))


;; (defun revy-ubertex-start ()
;;   "Start a sketch for the current ubertex buffer from the beginning.
;; This does not set the `revy-ubertex-mode' and is primarily used while working on the overtex file."
;;   (interactive)
;;   (goto-char (point-min))
;;   (revy-ubertex-restart))

(defun revy-ubertex-restart-from-here ()
  "(Re)start a sketch for the current ubertex buffer from the current point
This calls `revy-ubertex-mode' but continues from the current point.
Is primarily used while working on the overtex file."
  (interactive)
  (save-excursion
    (revy-ubertex-mode))
  ;; TODO: Ensure the correct slide is entered
  (revy-mode-enter))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-ubertex-forward ()
  "Moves point to next slide.
Does not affect the cursor."
  (interactive)
  (search-forward-regexp "\\\\begin{overtex}\n\\|\\\\pause\\({}\\)?" nil t))

(defun revy-ubertex-enter ()
  "Moves cursor to the slide where the point is located."
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

    (move-overlay revy-local-cursor (+ 7 start) end (current-buffer))
    (move-overlay revy-cursor (+ 7 start) end (current-buffer))
    ;(move-overlay revy-local-cursor (match-end 0) end (current-buffer))
    ;(move-overlay revy-cursor (match-end 0) end (current-buffer))
    (revy-pdf-goto-slide (revy-ubertex-slide-number))
    (revy-ubertex-scan start end)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Tex parsing/preparation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-ubertex-hide ()
  "Hides command tags in the buffer.
To make it easier to visually keep an overview.
Also does all the preparations for the buffer "
  (interactive)
  (revy-unhide)
  ;; (revy-ubertex-numerize)
  (revy-ubertex-insert-blank-comments)
  ;; (setq revy-ubertex-hidden '())
  (save-excursion
    ;; insert missing parens around elisp expressions
    (goto-char (point-min))
    (while (search-forward-regexp "\\\\elisp{(\\([^(][^)}]+\\)}" nil t)
       (replace-match "\\\\elisp{(\\1)}" nil nil))

    ;; Hide preamble and everything up until the first slide:
    (goto-char (point-min))
    (search-forward-regexp "\\\\begin{overtex}" nil t)
    (let ((overlay (make-overlay 0 (match-beginning 0) (current-buffer) t nil)))
      ;; (overlay-put overlay 'invisible t)
      (overlay-put overlay 'revy t)
      (overlay-put overlay 'face 'revy-hidden-face)
      ;; (push overlay revy-ubertex-hidden)
      )


    ;; Put overlays on tex to hide
    (goto-char (point-min))
    (let ((n 0))
      ;; Search over everything that needs overlays
      (while (search-forward-regexp "\\\\n{\\([0-9]+\\)}\\|\\\\begin{overtex}\\|\\\\end{overtex}\n\\|\\\\pause\\({}\\)?\\|^\n" nil t)
        ;; Create overlay
        (let ((overlay (make-overlay (match-beginning 0) (match-end 0) (current-buffer) t nil)))
          (overlay-put overlay 'revy t)
          (overlay-put overlay 'priority 9000)
          (overlay-put overlay 'face 'revy-hidden-face)
          ;(overlay-put overlay 'invisible t)

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
          (unless (or (string= (match-string 0) "\\pause")
                    (string= (match-string 0) "\\pause{}"))
            (message "kat")
            ;; (let ((icon (make-overlay (- (match-beginning 0) 1) (match-beginning 0) (current-buffer) t nil)))
            ;;       (overlay-put icon 'revy t)
            ;;       (overlay-put icon 'priority 9000)
            ;;       (overlay-put icon 'after-string "$")
            ;;       (push icon revy-ubertex-hidden))
;            (overlay-put overlay 'invisible t)
            )


          ;; (push overlay revy-ubertex-hidden)
         ;(put-text-property (match-beginning 0) (match-end 0) 'invisible t)
          )))))

(defun revy-ubertex-numerize ()
  "Numerates each slide by inserting a \n{...} tag at the beginning of each."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((n 0))
      (while (search-forward-regexp "\\\\begin{overtex}\\|\\\\pause\\({}\\)?" nil t)
      (insert "\\n{" (int-to-string n) "}")
      (incf n)))))

(defun revy-ubertex-unnumerize ()
  "Remove all numerating tags \n{...} in the buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "\\\\n{[0-9]+}" "")))


(defun revy-unhide ()
  "Unhides hidden text.
This restores the buffer to the default tex look."
  (interactive)
  (revy-ubertex-unnumerize))
  ;; (remove-text-properties (buffer-end -1) (buffer-end 1) 'bold)
  ;; (remove-overlays (buffer-end -1) (buffer-end 1) 'revy t)
  ;; (remove-overlays (buffer-end -1) (buffer-end 1) 'face 'revy-cursor-face))

(defun revy-ubertex-insert-blank-comments()
  "Turns empty comments into comments containing the line 'blank'.
This makes it easier to visually spot empty slides."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-string "\\begin{overtex}\n\\end{overtex}" "\\begin{overtex}\n%blank\n\\end{overtex}")))









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Ubertex slide handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun revy-ubertex-scan (start end)
;;   (interactive)

;;   (save-excursion
;;     (goto-char end)

;;     (let ((p (save-excursion (progn (search-backward-regexp "\\\\end{overtex}" nil t) (match-end 0)))))
;;       (when (not (= p end))
;;         (search-backward-regexp  "\\\\pause\\({}\\)?" nil t)))

;;     (search-backward-regexp "\\\\begin{overtex}\\|\\\\pause\\({}\\)?" nil t)

;;     (save-excursion
;;       (when (not (null (search-forward-regexp "\\\\shell{\\([^}]+\\)}" end t)))
;;         (message (match-string 1))
;;         (revy-shell (match-string 1))))
;;     (save-excursion
;;       (when (not (null (search-forward-regexp "\\\\elisp{(\\([^)}]+\\)}" end t)))
;;       (revy-elisp (match-beginning 1) (match-end 1))))))

(defun revy-ubertex-scan (start end)
  (interactive)
  ;; (print end)
  (save-excursion ;should end before evaluating code (so we can jump around)
    (goto-char end)
    (search-backward-regexp "\\\\begin{overtex}\\|\\\\pause[{}]?" nil t) ;; skriv regexes sådan
    (save-excursion
      (when (not (null (search-forward-regexp "\\\\shell{(\\([^}]*\\)}" end t)))
        ;(message (match-string 1))
        (revy-shell (match-string 1))))
    (save-excursion
      (when (not (null (search-forward-regexp "\\\\elisp{\\([^}]*\\)}" end t)))
        ;(message (match-string 1))
        (revy-elisp (match-beginning 1) (match-end 1))))))

  ;; (search-backward-regexp "\\\\pause\\({}\\)?" nil t)
  ;; (goto-char (match-beginning 0)))

  ;; (save-excursion)
  ;;     (search-backward-regexp "\\\\begin{overtex}"))
  ;;     )
      ;; (save-excursion
      ;;   (search-forward-regexp "\\\\shell{\\(.+\\)}" end t)
      ;;   (message (match-string 1))
      ;;   (revy-shell (match-string 1)))
      ;; (search-forward-regexp "\\\\elisp}\\(.+\\)}" end t)
      ;; (revy-elisp (match-string 1)))))




;; Doesn't work on overlay properties.
;; (defun revy-ubertex-slide-number ()
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


;;Works on revy-ubertex-numerize
(defun revy-ubertex-slide-number ()
  "Finds the slide number for the slide where point resides or the previous."
  (interactive)
  (save-excursion
    (search-backward-regexp "\\\\n{\\([0-9]+\\)}" nil t)
    (message (match-string 1))))

(defun revy-ubertex-slide-number ()
  "Finds the slide number for the slide where point resides or the previous."
  (interactive)
  (save-excursion
    ;; (let* ((overlays (overlays-at (- (point) 1)))
    ;;        (overlay (find nil overlays :test (lambda (_ o)
    ;;                                            (unless (null o)
    ;;                                              (overlay-get o 'revy-slide-number))))))
    ;;   (overlay-get overlay 'revy-slide-number))))

    (let ((overlays (overlays-at (- (point) 1)))
          overlay
          slide)
      (while overlays
        (setq overlay (car overlays))
        (let ((revy-slide-number (overlay-get overlay 'revy-slide-number)))
          (unless (null revy-slide-number)
            (setq slide revy-slide-number)
            (setq overlays nil))))
      slide)))




; (defun revy-ubertex-create-local-cursor ()
