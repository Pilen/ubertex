;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;π Ubercom
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handle communication
(provide 'revy-ubercom)


(defun revy-send-message (&rest args)
  "Send a message"
  (let* ((worker (if (revy-workerp (car args))
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

         (list (cons (revy-worker-get-name worker) (cons time args)))
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
  (when (not (null revy-leiter))
    (delete-process revy-leiter))
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
  ;; TODO: Refactor!!!
  "Evaluate shell command on a given worker.
If no worker is given/worker is nil,
the command will be executed on this machine."
  (save-window-excursion
    (if (null worker)
        (call-process-shell-command command nil 0)
      (let ((com
             ;; (concat "ssh "
             ;;                       (revy-worker-get-location worker)
             ;;                       " -T << EOF \n"
             ;;                       " export DISPLAY="
             ;;                       (revy-worker-get-display worker)
             ;;                       " ; "
             ;;                       command
             ;;                       " \n EOF ")))
             (concat "ssh " (revy-worker-get-location worker) " \""
                     "export DISPLAY=" (revy-worker-getdisplay worker) ";\n"
                     "cd " (revy-worker-get-dir worker) ";\n"
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
             " " (revy-worker-get-location revy-current-worker)
             ":" (revy-worker-get-dir revy-current-worker)
             subdir (file-name-nondirectory filename)))))

(defun revy-elisp (start &optional end)
  "Evaluates elisp code.
Default is to give it a region from start to end.
But it will also accept a string with end being ignored in that case."
  (if (stringp start)
      (if (string= start "")
          (message "<WARNING:> Evaluating empty string!")
        (eval (read start)))
    (eval-region start end)))

(defvar revy-syncing-files 0
  "Number off machines being synced to currently")

(defun revy-sync-files (&optional worker)
  "Sync local files to worker(s).
If a worker or a list of workers are supplied these workers are synced, else every worker is synced.
Syncs using rsync."
  ;; TODO: if rsync fails we might want to use scp (check exit code).

  ;; (revy-send-message "syncfiles")
  (let ((workers (if worker
                     (if (listp worker)
                         worker
                       (list worker))
                   (revy-worker-get-all-workers))))
    (print workers)
    (mapc
     (lambda (worker)
       (when (revy-worker-get-location worker)
         (lexical-let* ((name (revy-worker-get-name  worker))
                        (process (start-process (concat "revy-rsync-" name)
                                                (concat "*revy-rsync-" name "*")
                                                "rsync"
                                                "-r" "-u" "-P" "-e" "ssh"
                                                revy-dir
                                                (concat (revy-worker-get-location worker)
                                                        ":"
                                                        (revy-worker-get-dir worker)))))
           (incf revy-syncing-files)
           (message "Syncing: %s" name)
           (set-process-sentinel process
                                 (lambda (process event)
                                   (decf revy-syncing-files)
                                   (if (string= event "finished\n")
                                       (message "Sync with %s completed [%d left]" name revy-syncing-files)
                                     (message "Sync with %s failed with exit code %i [%d left]"
                                              name (process-exit-status process) revy-syncing-files)))))))
     workers)))
