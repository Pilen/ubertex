;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;π Ubersicht
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'revy-ubersicht)


(defun revy-ubersicht-next ()
  "Enter the item after the current"
  (interactive)
  (goto-char (overlay-end revy-local-cursor))
  (revy-ubersicht-enter))

(defun revy-ubersicht-enter ()
  "Enter an item
By default the one the point is located in"
  (interactive)
  (let ((orig (point))
        (start nil)
        (end nil))
    (setq start (search-backward-regexp "^(" nil t))

    (if (null start)
        ;; We are at the start of the buffer and should evaluate the first slide
        ;; We therefore say that the end of the (non-existing) previous slide is before the point.
        (setq end 0)
      ;; goto end of current slide
      (goto-match-paren)
      (setq end (point)))

    (when (>= orig end)
      ;; Point is located outside previous instruction
      ;; So we should move forward and evaluate the next instruction
      (search-forward-regexp "^(" nil t) ;; Fails from last slide, continuously marking last slide (except last paren).
      (backward-char)
      (setq start (point))
      (goto-match-paren)
      (setq end (point)))

    (move-overlay revy-local-cursor start end (current-buffer))
    (move-overlay revy-cursor start end (current-buffer))
    (eval-region start end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;π Overall
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode revy-ubersicht-mode emacs-lisp-mode "Ubersicht"
  "Major mode for ubersicht buffers
Based off `emacs-lisp-mode'"

  ;; Clear old cursor
  (when (not (null revy-local-cursor))
    (delete-overlay revy-local-cursor)
    (setq revy-local-cursor nil))

  ;; Create new local cursor
  (setq revy-local-cursor (make-overlay 0 0 (current-buffer) t t))
  (overlay-put revy-local-cursor 'face 'revy-local-cursor-face)
  (overlay-put revy-local-cursor 'priority 4999)
  (overlay-put revy-local-cursor 'revy t)

  (goto-char (point-min)))

(add-to-list 'auto-mode-alist '("\\.revy\\'" . revy-ubersicht-mode))

(define-key revy-ubersicht-mode-map (kbd "<next>") 'revy-ubersicht-next)
(define-key revy-ubersicht-mode-map (kbd "<end>") 'revy-ubersicht-enter)
