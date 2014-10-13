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

  ;; Go to beginning
  (goto-char (point-min))

  ;; Setup metafunctions
  (setq revy-mode-enter-function 'revy-ubertex-enter)
  (setq revy-mode-forward-function 'revy-forward-enter)

  ;; Clear old cursors
  (revy-clear-overlays)

  ;; Create new local cursor
  (setq revy-local-cursor (make-overlay 0 0 (current-buffer) t t))
  (overlay-put revy-local-cursor 'revy t)
  (overlay-put revy-local-cursor 'priority 4999)
  (overlay-put revy-local-cursor 'face 'revy-local-cursor-face)

  ;; Prepare buffer.
  (revy-ubertex--prepare)

  ;; Transfer pdf
  ;; (revy-sync-files)
  ;; (let ((filename (concat (file-name-sans-extension (buffer-file-name)) ".pdf")))
  ;;   (when revy-scp-mode
  ;;     (revy-scp-file filename "pdfs")))

  ;; Open pdf on worker
  (revy-pdf-open (revy-data-path (buffer-file-name) ".pdf")))


(defun revy-ubertex-start-from-here ()
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

    (search-forward-regexp "\\\\end{overtex}\\|\\\\pause\\({}\\)?" nil t)
    (setq end (match-end 0))

    (save-excursion
      (search-backward-regexp "\\\\begin{overtex}" nil t)
      (setq start (match-end 0))
      (when (char-equal (char-after start) ?\n)
        (incf start)))

    (move-overlay revy-local-cursor start end (current-buffer))
    (move-overlay revy-cursor start end (current-buffer))
    (revy-pdf-goto-slide (revy-ubertex--slide-number))
    (revy-ubertex--scan end)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Tex parsing/preparation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-ubertex--prepare ()
  "Hides command tags in the buffer.
To make it easier to visually keep an overview.
Also does all the preparations for the buffer "
  (save-excursion

    ;; Turns empty comments into comments containing the line 'blank'.
    ;; This makes it easier to visually spot empty slides.
    (replace-string "\\begin{overtex}\n\\end{overtex}"
                    "\\begin{overtex}\n%blank\n\\end{overtex}"
                    nil (point-min) (point-max))

    ;; insert missing parens around elisp expressions
    (goto-char (point-min))
    (while (search-forward-regexp "\\\\elisp{(\\([^(][^)}]+\\)}" nil t)
       (replace-match "\\\\elisp{(\\1)}" nil nil))

    ;; Hide preamble and everything up until the first slide:
    (goto-char (point-min))
    (search-forward-regexp "\\\\begin{overtex}" nil t)
    (let ((overlay (make-overlay 0 (match-beginning 0) (current-buffer) t nil)))
      (overlay-put overlay 'revy t)
      (overlay-put overlay 'face 'revy-hidden-face))

    ;; Put overlays on tex to hide
    (goto-char (point-min))
    (let ((n 0))
      ;; Search over everything that needs overlays
      (while (search-forward-regexp "\\\\begin{overtex}\\|\\\\end{overtex}\n\\|\\\\pause\\({}\\)?\\|^\n" nil t)
        ;; Create overlay
        (let ((overlay (make-overlay (match-beginning 0) (match-end 0) (current-buffer) t nil)))
          (overlay-put overlay 'revy t)
          (overlay-put overlay 'priority 9000)
          (overlay-put overlay 'face 'revy-hidden-face)

          (when (member (match-string 0) '("\\end{overtex}\n" "\\pause" "\\pause{}"))
            (overlay-put overlay 'revy-slide-number n)
            (incf n))))
      n)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Ubertex slide handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun revy-ubertex--scan (start end)
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

(defun revy-ubertex--scan (end)
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

(defun revy-ubertex--scan (end)
  (let ((code nil))
    (save-excursion
      (goto-char end)
      (search-backward-regexp "\\\\begin{overtex}\\|\\\\pause[{}]?" nil t)
      (save-excursion
        (while (search-forward-regexp "\\\\shell{\\([^}]*\\)}" end t)
          (revy-shell (match-string 1))))

      ;; Collect all lisp code
      (while (search-forward-regexp "\\\\elisp{\\([^}]*\\)}" end t)
        ;; Dont evaluate in save-excursion, so we can jump around
        (message "fisk")
        (push (match-string 1) code)))

    ;; Evaluate lisp code in correct order
    (setq code (nreverse code))
    (while code
      (revy-elisp (car code))
      (setq code (cdr code)))))


(defun revy-ubertex--slide-number ()
  "Finds the slide number for the slide where point resides or the previous."
  (interactive)
  ;; TODO; decide whether to search back first
  (let ((overlays (overlays-at (- (point) 1)))
        overlay
        slide)
    ;; Search through overlays for one with the 'revy-slide-number property
    (while overlays
      (setq overlay (car overlays))
      (setq overlays (cdr overlays))
      (let ((revy-slide-number (overlay-get overlay 'revy-slide-number)))
        (unless (null revy-slide-number)
          ;; End loop and return slide number
          (setq slide revy-slide-number)
          (setq overlays nil))))
    slide))
