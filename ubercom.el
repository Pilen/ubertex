
(defun revy-send-message (&rest args)
  "Send a message"
  (let* ((worker (if (revy-worker-p (car args))
                     (pop args)
                   revy-current-worker))
         (time (cond ((integerp (car args))
                      (int-to-string (pop args)))
                     ((string-equal "" (car args))
                      (pop args))
                     ((string-equal "now" (car args))
                      (pop args))
                     (t
                      "now")))

         (list (cons (revy-worker-name worker) (cons time args)))
         (message (mapconcat (lambda (x) (if (integerp x) (int-to-string x) x))
                             list ";"))
         (quoted (concat (replace-regexp-in-string "\n" "\\\\n" message) "\n")))

    (message quoted)
    ;;do something with the above

    ;; When leiter is not running start it
    (when (or (not (processp revy-leiter))
              (not (process-live-p revy-leiter)))
      (revy-start-leiter))
    (process-send-string revy-leiter quoted)))

(defun revy-start-leiter ()
  "Start leiter.
If leiter is allready running, kill it first, and restart it."
  (delete-process revy-leiter)
  (let ((process-connection-type nil)) ;; Use pipes
    (setq revy-leiter
          (start-process "leiter" "*leiter*"
                         (concat (file-name-as-directory revy-ubertex-dir)
                                 "tools/leiter.py")))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Simple work
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Should concat by itself
(defun revy-shell (command &optional worker)
  "Evaluate shell command on a given worker.
If no worker is given/worker is nil,
the command will be executed on this machine."
  (save-window-excursion
    (if (null worker)
        (call-process-shell-command command nil 0)
      (let ((com
             ;; (concat "ssh "
             ;;                       (revy-worker-location worker)
             ;;                       " -T << EOF \n"
             ;;                       " export DISPLAY="
             ;;                       (revy-worker-display worker)
             ;;                       " ; "
             ;;                       command
             ;;                       " \n EOF ")))
             (concat "ssh " (revy-worker-location worker) " \""
                     "export DISPLAY=" (revy-worker-display worker) ";\n"
                     "cd " (revy-worker-dir worker) ";\n"
                     command
                     ";\"")))
        ;(print com)
        (call-process-shell-command com nil 0)))))
        ;(start-process-shell-command "ubertex" nil com)))))
        ;(async-shell-command com nil nil)))))

;; Might be unnecessary if using sshmount
(defun revy-scp-file (filename subdir)
  "Copy a file to a worker using scp"
  (when revy-scp-mode
    (revy-shell
     (concat "scp "
             filename
             " " (revy-worker-location revy-current-worker)
             ":" (revy-worker-dir revy-current-worker)
             subdir (file-name-nondirectory filename)))))

(defun revy-elisp (start &optional end)
  "Evaluates elisp code.
Default is to give it a region from start to end.
But it will also accept a string with end being ignored in that case."
  (if (stringp start)
      (eval (read (start)))
    (eval-region start end)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;π Commands
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; All commands are based on the current worker `revy-current-worker'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-sync-files (&optional worker)
  "Sync local files to worker(s).
If a worker is supplied this worker is synced, else every worker is synced."
  (revy-send-message "syncfiles"))


(defun revy-blank ())
(defun revy-blank-all ())
(defun revy-unblank-all ())

(defun revy-abort ())
(defun revy-abort-all ())


(defun revy-kill-sketch ())



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π PDF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-pdf-open (file)
  "Open a PDF file"
  (revy-send-message "start" "PDF" 0 0 "sized" file))

(defun revy-pdf-reload ()
  "Reload current pdf"
  (revy-send-message "module" "PDF" "reload"))

(defun revy-pdf-goto-slide (slide)
  "Goto pdf slide."
  (revy-send-message "module" "PDF" "goto" slide))

(defun revy-pdf-next ()
  "Goto next slide"
  (revy-send-message "module" "next"))
