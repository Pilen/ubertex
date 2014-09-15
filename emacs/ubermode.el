;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;π Ubermode
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'revy-ubermode)

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
  "Enter the item/instruction/slide the cursor is located in or, if outside any, the following.

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

    (save-excursion
      (goto-char (overlay-end revy-local-cursor))
      (revy-mode-enter))

    (unless (null revy-follow-cursor)
        (goto-char (overlay-end revy-local-cursor))
        (unless (eq revy-follow-cursor 'follow)
          (recenter)))))



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
