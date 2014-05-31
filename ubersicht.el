;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;π Ubersicht
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-ubersicht-next ()
  (interactive)
  (goto-char (overlay-end revy-local-cursor))
  (revy-ubersicht-enter))
  ;; (search-forward-regexp "^(")
  ;; (backward-char)
  ;; (let  ((start (point)))
  ;;   (goto-match-paren)
  ;;   (move-overlay revy-local-cursor start (point) (current-buffer))
  ;;   (move-overlay revy-cursor start (point) (current-buffer))
  ;;   (eval-region start (point) standard-output)))

;; Should this behave more like revy-enter?
(defun revy-ubersicht-enter ()
  (interactive)
  ;; (let ((start (search-backward-regexp "^(")))
  ;;   (goto-match-paren)
  ;;   (eval-region start (point) standard-output)))
  (let ((orig (point))
        (start nil)
        (end nil))
    (setq start (search-backward-regexp "^(" nil t))

    (if (null start)
        ;; We are at the start of the buffer  and should evaluate the first slide
        (setq end 0)
      ;; goto end of current slide
      (goto-match-paren)
      (setq end (point)))

    (if (< orig end)
        (progn
          (print "<- eval current")
          (move-overlay revy-local-cursor start end (current-buffer))
          (move-overlay revy-cursor start end (current-buffer))
          (eval-region start end))
      (progn
        (print "-> eval next")
        (search-forward-regexp "^(" nil t) ;; Fails from last slide, continuously marking last slide (except last paren).
        (backward-char)
        (setq start (point))
        (goto-match-paren)
        (move-overlay revy-local-cursor start (point) (current-buffer))
        (move-overlay revy-cursor start (point) (current-buffer))
        (eval-region start (point))
        )

    ))
  ;; (search-forward-regexp "^(")
  ;; (backward-char)
  ;; (goto-match-paren)
  )

(defun revy-open (filename &optional screen)
  (message "running revy-open!")
  (push (current-buffer) revy-stack)
  (find-file-other-window filename)
  (when screen
    (setq revy-default-screen screen))
  (if (string= (downcase (file-name-extension filename)) "tex")
      (progn (revy-manus-mode t)
             (revy-ubertex-mode t))
    (revy-ubersicht-mode t)))

(defun revy-nop (&optional &rest _))

(defun revy-quit ()
  (interactive)
  (revy-unhide)
  (revy-kill)
  (pop-to-buffer (pop revy-stack)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;π Overall
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-start ()
  (interactive)
  (setq revy-stack '())
  (push (current-buffer) revy-stack)
  (revy-ubersicht-mode t))


(define-minor-mode revy-ubersicht-mode
  "Minor mode for ubersicht buffers"
  :lighter "ubersicht"
  :keymap (let ((revy-ubersicht-mode-map (make-sparse-keymap)))
            (define-key revy-ubersicht-mode-map (kbd "<next>") 'revy-ubersicht-next)
            (define-key revy-ubersicht-mode-map (kbd "<end>") 'revy-ubersicht-enter)
            revy-ubersicht-mode-map)
  (when (not revy-ubersicht-mode)
    (revy-unhide))
  (when (not (null revy-local-cursor))
    (delete-overlay revy-local-cursor)
    (setq revy-local-cursor nil))
  (when revy-ubersicht-mode
    (setq revy-local-cursor (make-overlay 0 0 (current-buffer) t t))
    (overlay-put revy-local-cursor 'face 'revy-local-cursor-face)
    (overlay-put revy-local-cursor 'priority 4999)
    (overlay-put revy-local-cursor 'revy t)
    (message "kamel")
    (goto-char (point-min)))
  )
