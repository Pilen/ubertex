;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;π Ubermode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'revy-ubermode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Revy-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-minor-mode revy-mode
  "Minor mode for revy.
This mode provides an interface over the various modes in the revy system.
Its primary focus is to provide a centralized set of keybindings to create a
uniform interface."
  :lighter "revy-mode"
  :keymap (let ((revy-mode-map (make-sparse-keymap)))
            (define-key revy-mode-map (kbd "<home>") 'revy-mode-next )
            (define-key revy-mode-map (kbd "<end>") 'revy-mode-enter)
            (define-key revy-mode-map (kbd "<delete>") 'revy-blank)

            (define-key revy-mode-map (kbd "<f11>") 'revy-mode-point-backward)
            (define-key revy-mode-map (kbd "<f12>") 'revy-mode-point-forward)
            (define-key revy-mode-map (kbd "<insert>") 'revy-mode-enter)

            (define-key revy-mode-map (kbd "<XF86Launch1>") 'revy-abort-all)
            (define-key revy-mode-map (kbd "<f10>") 'revy-ubermenu)
            ;; (define-key revy-mode-map (kbd "<f10>") (lambda () (interactive) (save-buffer) (revy-compile-tex) (revy-upload-files-sync) (revy-ubertex-mode)))
            revy-mode-map))

(setq revy-uberclear? t)
;; (define-key revy-mode-map (kbd "<next>") 'revy-mode-next)
;; (global-set-key (kbd "<print>") (lambda () (interactive) (if revy-uberclear? (revy-clear-overlays) (revy-ubertex-mode)) (setq revy-uberclear? (not revy-uberclear?))))
;; (global-set-key (kbd "<Scroll_Lock>") (lambda () (interactive) (revy-upload-files)))
;; (global-set-key (kbd "<XF86AudioMicMute>") (lambda () (interactive) (revy-upload-files revy-current-worker) (sleep-for 1) (revy-ubertex-mode)))

(define-global-minor-mode global-revy-mode revy-mode
  (lambda ()
    (when (member major-mode '(revy-ubersicht-mode revy-ubertex-mode revy-ubersong-mode))
      (revy-mode))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Meta function variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar revy-mode-enter-function nil
  "The function to call on `revy-mode-enter.
Use `nil' for default behaviour.")
(make-variable-buffer-local 'revy-mode-enter-function)

(defvar revy-mode-next-function nil
  "The function to call on `revy-mode-next.
Use `nil' for default behaviour.")
(make-variable-buffer-local 'revy-mode-next-function)

(defvar revy-mode-point-forward-function nil
  "The function to call on `revy-mode-point-forward.
Use `nil' for default behaviour.")
(make-variable-buffer-local 'revy-mode-point-forward-function)

(defvar revy-mode-point-backward-function nil
  "The function to call on `revy-mode-point-backward.
Use `nil' for default behaviour.")
(make-variable-buffer-local 'revy-mode-point-backward-function)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Meta functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun revy-mode-enter ()
  "Enter the item/instruction/slide the cursor is located in or, if outside any,
  the following.

This function is mode generic but can be specialized in modes by
setting a function in `revy-mode-enter-function'
By default it doesn't do anything."
  (interactive)
  (if (not (or (null revy-mode-enter-function)
               (eq revy-mode-enter-function 'revy-mode-enter-function)))
      (funcall revy-mode-enter-function)

    (message "No default behaviour")
    nil))


(defun revy-mode-next ()
  "Enter the item/instruction/slide after the current

This function is mode generic but can be specialized in modes by
setting a function in `revy-mode-next-function'"
  (interactive)
  (if (not (or (null revy-mode-next-function)
               (eq revy-mode-next-function 'revy-mode-next-function)))
      (funcall revy-mode-next-function)

    (let ((window (get-buffer-window)))
      (save-excursion
        (goto-char (overlay-end revy-local-cursor))
        (revy-mode-enter))

      (unless (eq (current-buffer)
                  (overlay-buffer revy-cursor))
        (pop-to-buffer (overlay-buffer revy-cursor)))

      (when revy-follow-cursor
        (goto-char (overlay-end revy-local-cursor))
        (unless (eq revy-follow-cursor 'follow)
          (with-selected-window window
            (recenter)))))))



(defun revy-mode-point-forward ()
  "Moves point to next item/instruction/slide
Does not move the cursor or evaluate anything

This function is mode generic but can be specialized in modes by
setting a function in `revy-mode-point-forward-function'
By default it doesn't do anything."
  (interactive)
  (if (not (or (null revy-mode-point-forward-function)
               (eq revy-mode-point-forward-function 'revy-mode-point-forward-function)))
      (funcall revy-mode-point-forward-function)

    (message "No default behaviour")
    nil))


(defun revy-mode-point-backward ()
  "Moves point to previous item/instruction/slide
Does not move the cursor or evaluate anything

This function is mode generic but can be specialized in modes by
setting a function in `revy-mode-point-backward-function'
By default it doesn't do anything."
  (interactive)
  (if (not (or (null revy-mode-point-backward-function)
               (eq revy-mode-point-backward-function 'revy-mode-point-backward-function)))
      (funcall revy-mode-point-backward-function)

    (message "No default behaviour")
    nil))
