;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;π COMMAND-CENTER
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'revy-command-center)

(defvar revy-command-center-commands '())
(defvar revy-command-center-last-command nil)

(defun revy-command-center-add (command &optional name)
  (when (null name)
    (if (symbolp command)
        (setq name (symbol-name command))
      (setq name (prin1-to-string command))))

  (setq revy-command-center-commands (append
                                 revy-command-center-commands
                                 `((,name . ,command)))))

(defun revy-command-center-clear ()
  (setq revy-command-center-commands '()))

(defun revy-command-center ()
  (interactive)
  (let* ((name (ido-completing-read
                ">: "
                (mapcar (lambda (x) (car x))
                        (if (null revy-command-center-last-command)
                            revy-command-center-commands
                          (cons revy-command-center-last-command revy-command-center-commands)))))
         (command (cdr (assoc name revy-command-center-commands))))
    (setq revy-command-center-last-command (cons name command))
    (if (commandp command)
        (call-interactively command)
      (funcall command))))

(progn
  (revy-command-center-clear)
  (revy-command-center-add 'revy-quit)
  (revy-command-center-add 'revy-abort-all)
  (revy-command-center-add 'revy-abort)
  (revy-command-center-add 'revy-stop-sounds)
  (revy-command-center-add 'revy-show-text)
  (revy-command-center-add 'revy-ubertex-mode)
  (revy-command-center-add 'revy-ubersicht-mode)
  (revy-command-center-add 'revy-sync-files)
  (revy-command-center-add 'revy-manus-break)
  (revy-command-center-add 'revy-manus-comment)
  (revy-command-center-add 'revy-manus-preamble)
  (revy-command-center-add 'revy-manus-clean)
  (revy-command-center-add (lambda () (interactive) (insert "\\pause{}")) "revy manus \\pause{}")

  (revy-command-center-add 'revy-blank)
  (revy-command-center-add 'revy-blank-all)
  (revy-command-center-add 'revy-unblank-all)
  (revy-command-center-add 'revy-restart)
  (revy-command-center-add 'revy-load)
  (revy-command-center-add 'revy-create)

  (revy-command-center-add 'revy-mode-enter)
  (revy-command-center-add 'revy-mode-next)

  (revy-command-center-add 'rename-file-and-buffer)
  (revy-command-center-add 'move-buffer-file)
  (revy-command-center-add 'emacs-lisp-mode)
  (revy-command-center-add 'delete-file)
  (revy-command-center-add 'whitespace-cleanup)
  (revy-command-center-add 'reindent-buffer)
  (revy-command-center-add 'occur)
  (revy-command-center-add 'indent-region)
  (revy-command-center-add 'make-directory)
  (revy-command-center-add 'shell-toggle-cd "eshell-cd")
  (revy-command-center-add 'delete-other-frames "frame-close-others")
  (revy-command-center-add 'make-frame-command "frame-new")
  (revy-command-center-add 'delete-frame "frame-close")
  (revy-command-center-add (lambda () (interactive) (untabify (point-min) (point-max))) "untabify-buffer")
  (revy-command-center-add 'sudo-edit-current-file "edit current file sudo root")
  (revy-command-center-add 'ediff)
  (revy-command-center-add 'ediff-buffers)
  (revy-command-center-add 'ediff-directories)
  (revy-command-center-add 'increment-number-at-point)
  (revy-command-center-add 'decrement-number-at-point)
  )
