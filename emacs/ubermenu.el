;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;π UBERMENU
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'revy-ubermenu)

(defvar revy-ubermenu-commands '())


(defun revy-ubermenu-clear ()
  (setq revy-ubermenu-commands '()))

(defun revy-ubermenu-add (command &optional name)
  (unless name
    (setq name (symbol-name command)))
  (add-to-list 'revy-ubermenu-commands (cons name command) t))

(defun revy-ubermenu ()
  (interactive)
  (let* ((name (ido-completing-read
                ">: "
                (mapcar #'car revy-ubermenu-commands)))
         (command (cdr (assoc name revy-ubermenu-commands)))
         (cell (cons name command)))
    (setq revy-ubermenu-commands (cons cell (delete cell revy-ubermenu-commands)))
    (if (commandp command)
        (call-interactively command)
      (funcall command))))

(progn
  (revy-ubermenu-clear)
  (revy-ubermenu-add 'revy-load)
  (revy-ubermenu-add 'revy-abort-all)
  (revy-ubermenu-add 'revy-build)
  (revy-ubermenu-add 'revy-end-sketch)
  (revy-ubermenu-add 'revy-abort)
  (revy-ubermenu-add 'revy-stop-sounds)
  (revy-ubermenu-add 'revy-fade-sounds)
  (revy-ubermenu-add 'revy-show-text)
  (revy-ubermenu-add 'revy-ubertex-mode)
  (revy-ubermenu-add 'revy-ubersicht-mode)
  (revy-ubermenu-add 'revy-upload-files)
  (revy-ubermenu-add 'revy-manus-break)
  (revy-ubermenu-add 'revy-manus-insert-comment)
  (revy-ubermenu-add 'revy-manus-prepare)
  (revy-ubermenu-add 'revy-manus-clean)
  (revy-ubermenu-add (lambda () (interactive) (insert "\\pause{}")) "revy manus \\pause{}")

  (revy-ubermenu-add 'revy-blank)
  (revy-ubermenu-add 'revy-blank-all)
  (revy-ubermenu-add 'revy-unblank-all)
  (revy-ubermenu-add 'revy-restart)
  (revy-ubermenu-add 'revy-load)
  (revy-ubermenu-add 'revy-create)
  (revy-ubermenu-add 'revy-compile-tex)
  (revy-ubermenu-add 'revy-test-all)

  (revy-ubermenu-add 'revy-mode-enter)
  (revy-ubermenu-add 'revy-mode-next)

  (revy-ubermenu-add 'eshell)
  (revy-ubermenu-add 'save-some-buffers "save-all-buffers")
  (revy-ubermenu-add 'save-buffer)
  (revy-ubermenu-add 'isearch-forward)
  (revy-ubermenu-add 'isearch-backward)
  (revy-ubermenu-add 'replace-string)
  (revy-ubermenu-add 'other-window)
  (revy-ubermenu-add 'ido-switch-buffer)
  (revy-ubermenu-add 'ibuffer)
  (revy-ubermenu-add 'undo)

  (revy-ubermenu-add 'kill-line)
  (revy-ubermenu-add 'kill-whole-line)

  (revy-ubermenu-add 'previous-line)
  (revy-ubermenu-add 'left-char)
  (revy-ubermenu-add 'next-line)
  (revy-ubermenu-add 'right-char)
  (revy-ubermenu-add 'beginning-of-line)
  (revy-ubermenu-add 'end-of-line)
  (revy-ubermenu-add 'scroll-down-command)

  (revy-ubermenu-add 'revert-buffer)

  (revy-ubermenu-add 'next-buffer)
  (revy-ubermenu-add 'previous-buffer)

  (revy-ubermenu-add 'join-line)

  (revy-ubermenu-add 'rename-file-and-buffer)
  (revy-ubermenu-add 'move-buffer-file)
  (revy-ubermenu-add 'emacs-lisp-mode)
  (revy-ubermenu-add 'delete-file)
  (revy-ubermenu-add 'whitespace-cleanup)
  (revy-ubermenu-add 'reindent-buffer)
  (revy-ubermenu-add 'occur)
  (revy-ubermenu-add 'indent-region)
  (revy-ubermenu-add 'make-directory)
  (revy-ubermenu-add 'shell-toggle-cd "eshell-cd")
  (revy-ubermenu-add 'delete-other-frames "frame-close-others")
  (revy-ubermenu-add 'make-frame-command "frame-new")
  (revy-ubermenu-add 'delete-frame "frame-close")
  (revy-ubermenu-add (lambda () (interactive) (untabify (point-min) (point-max))) "untabify-buffer")
  (revy-ubermenu-add 'sudo-edit-current-file "edit current file sudo root")
  (revy-ubermenu-add 'ediff)
  (revy-ubermenu-add 'ediff-buffers)
  (revy-ubermenu-add 'ediff-directories)
  (revy-ubermenu-add 'increment-number-at-point)
  (revy-ubermenu-add 'decrement-number-at-point)
  )
