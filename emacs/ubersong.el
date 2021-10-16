;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;π Ubersong
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'revy-ubersong)

;; use make-local-variable to make it buffer local when needed, dont do it globally. That is just confusing when rarely used
(defvar-local revy-ubersong-text-prefix "")
(defvar-local revy-ubersong-text-postfix "")
(defvar-local revy-ubersong-text-alignment nil)
(defvar-local revy-ubersong-text-position nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Major mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (setq revy-ubersong-mode-syntax-table (make-syntax-table sgml-syntax-table))

(setq revy-ubersong-mode-syntax-table
      (let ((table (make-syntax-table)))
        (modify-syntax-entry ?< "(>" table)
        (modify-syntax-entry ?> ")<" table)
        table))


(define-derived-mode revy-ubersong-mode fundamental-mode "Ubersong"
  "Major mode for uber song buffers."
  :syntax-table revy-ubersong-mode-syntax-table

  ;; Go to beginning
  (goto-char (point-min))

  ;; Setup metafunctions
  (setq revy-mode-forward-function 'revy-ubersong-point-forward)
  (setq revy-mode-enter-function 'revy-ubersong-enter)

  ;; Clear old cursors
  (revy-clear-overlays)

  ;; Create new local cursor
  (setq revy-local-cursor (make-overlay 0 0 (current-buffer) t nil))
  (overlay-put revy-local-cursor 'revy t)
  (overlay-put revy-local-cursor 'priority 4999)
  (overlay-put revy-local-cursor 'face 'revy-local-cursor-face)

  ;; Prepare buffer.
  (revy-ubersong--prepare)

  ;; Clean up on mode change
  (add-hook 'change-major-mode-hook #'revy-clear-overlays nil t))


(defun revy-ubersong--prepare ()
  "Hides command tags in the buffer.
To make it easier to visually keep an overview."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "<<\\|>>" nil t)
      (let ((overlay (make-overlay (match-beginning 0) (match-end 0) (current-buffer) t nil)))
        (overlay-put overlay 'revy t)
        (overlay-put overlay 'priority 9000)
        (overlay-put overlay 'face 'revy-hidden-face)))))

(defun revy-ubersong-point-forward ()
  "Moves point to next slide.
Does not affect the cursor."
  (interactive)
  (search-forward-regexp ">>\n?" nil t))

(defun revy-ubersong-enter ()
  "Moves cursor to the slide where the point is located."
  (interactive)
  (let ((start nil) ;; Start of slide
        (end nil) ;; Where we want to end the display of slide
        (final nil) ;; Where the slide ends
        text)
    (search-forward-regexp ">>\n?" nil t)
    (setq end (match-end 0))

    (save-excursion
      (search-backward-regexp "^<<" nil t)
      (setq start (match-end 0))
      (when (char-equal (char-after start) ?\n)
        (incf start))

      (forward-list)
      (setq final (- (point) 2))
      (when (char-equal (char-before final) ?\n)
        (decf final)))

    (move-overlay revy-local-cursor start end (current-buffer))
    (move-overlay revy-cursor start end (current-buffer))

    ;; Do the actual parsing
    (setq text
          (replace-regexp-in-string
           "<<>>\\|\\(<([^>]*)>[\n]?\\)" ""
           (if (>= end final)
               (buffer-substring-no-properties start final)
             (format "%s<span alpha='1'>%s</span>"
                     (buffer-substring-no-properties start end)
                     (buffer-substring-no-properties end final)))))
    (setq text (concat revy-ubersong-text-prefix text revy-ubersong-text-postfix))
    (message "%d %d %d %d" start end final (point))
    ;; (revy-send-lisp nil `(progn (set 'current-text ,text) (update (text 'current-text nil ,revy-ubersong-text-alignment ,revy-ubersong-text-position))))
    (revy-send-lisp nil `(update (text ,text nil ,revy-ubersong-text-alignment ,revy-ubersong-text-position)))

    (revy-ubersong--scan start end)))


(defun revy-ubersong--scan (start end)
  (let ((code nil)
        from)
    (save-excursion
      (goto-char end)
      (unless (search-backward-regexp ">>" start t 2)
        (goto-char start))
      (while (search-forward-regexp "<(" end t)
        (backward-char)
        (setq from (point))
        (forward-list)
        (push (buffer-substring-no-properties from (point)) code)))

    (setq code (nreverse code))
    (while code
      (revy-elisp (car code))
      (setq code (cdr code)))))
