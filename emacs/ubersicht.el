;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;π Ubersicht
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'revy-ubersicht)


(define-derived-mode revy-ubersicht-mode emacs-lisp-mode "Ubersicht"
  "Major mode for ubersicht buffers
Based off `emacs-lisp-mode'"

  ;; Clear old cursors
  (revy-clear-overlays)

  ;; Create new local cursor
  (setq revy-local-cursor (make-overlay 0 0 (current-buffer) t t))
  (overlay-put revy-local-cursor 'revy t)
  (overlay-put revy-local-cursor 'priority 4999)
  (overlay-put revy-local-cursor 'face 'revy-local-cursor-face)

  ;; Setup metafunctions
  (setq revy-mode-enter-function 'revy-ubersicht-enter)

  ;; Clean up on mode change
  (add-hook 'change-major-mode-hook #'revy-clear-overlays nil t)

  ;; Go to beginning
  (goto-char (point-min)))

(add-to-list 'auto-mode-alist '("\\.revy\\'" . revy-ubersicht-mode))
(add-to-list 'auto-mode-alist '("\\.sketch\\'" . revy-ubersicht-mode))


(defun revy-ubersicht-enter ()
  "Enter an instruction
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
