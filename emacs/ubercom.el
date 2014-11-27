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
  "Evaluate shell command on a given worker asynchronously.
If no worker is given/worker is nil,
the command will be executed on revy-current-worker."
  (save-window-excursion
    (when (null worker)
      (setq worker revy-current-worker))
    (start-process "revy-shell" "*revy-shell*"
                   "ssh" (revy-worker-get-location worker)
                   (concat "export DISPLAY=" (revy-worker-get-display worker) ";\n"
                           "cd " (revy-worker-get-dir worker) ";\n"
                           command))))

(defun revy-shell-sync (command &optional worker)
  ;; Todo, does not seem to work with current worker?!
  "Evaluate shell command on a given worker synchronously.
If no worker is given/worker is nil,
the command will be executed on revy-current-worker."
  (save-window-excursion
    (with-current-buffer (get-buffer-create "*revy-shell*")
      (goto-char (point-max))
      (when (null worker)
        (setq worker revy-current-worker))
      (call-process "ssh" nil "*revy-shell*" t
                    (revy-worker-get-location worker)
                    (concat "export DISPLAY=" (revy-worker-get-display worker) ";\n"
                            "cd " (revy-worker-get-dir worker) ";\n"
                            command)))))

(defun revy-shell-local (command)
  "Evaluate shell command on this machine asynchronously."
  (save-window-excursion
    (start-process-shell-command "revy-shell" "*revy-shell*" command)))

(defun revy-shell-sync-local (command)
  "Evaluate shell command on this machine synchronously."
  (save-window-excursion
    (with-current-buffer (get-buffer-create "*revy-shell*")
      (goto-char (point-max))
      (call-process-shell-command command nil "*revy-shell*" t))))

;; Might be unnecessary if using sshmount
(defun revy-scp-file (filename subdir)
  "Copy a file to a worker using scp"
  (when revy-scp-mode
    (revy-shell
     (concat "scp "
             filename
             " " (revy-worker-get-location revy-current-worker)
             ":" (revy-worker-get-dir revy-current-worker)
             (file-name-as-directory subdir) (file-name-nondirectory filename)))))

(defun revy-elisp (start &optional end)
  "Evaluates elisp code.
Default is to give it a region from start to end.
But it will also accept a string with end being ignored in that case."
  (if (stringp start)
      (if (string= start "")
          (message "<WARNING:> Evaluating empty string!")
        (eval (read start)))
    (eval-region start end)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Uploading files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar revy-syncing-files 0
  "Number off machines being synced to currently")

(defun revy-upload-files (&rest workers)
  ;; Todo fix dokumentation
  "Upload files to workers.
If no workers are specified, the files will be uploaded to all workers.
Remember only nonvirtual workers are updated

Uses rsync to upload the files, based on the timestamp"

  (interactive)
  ;; TODO: if rsync fails we might want to use scp (check exit code).

  ;; (revy-send-message "syncfiles")
  (unless workers
    (setq workers (revy-worker-get-all-workers)))

  (dolist (worker workers)
    ;; Only sync nonvirtual workers
    (when (revy-worker-get-location worker)
      (lexical-let* ((name (revy-worker-get-name  worker))
                     (process (start-process (concat "revy-rsync-" name)
                                             (concat "*revy-rsync-" name "*")
                                             "rsync"
                                             "-r" "-u" "-t" "-P" "-e" "ssh"
                                             (file-name-as-directory revy-dir)
                                             (concat (revy-worker-get-location worker)
                                                     ":"
                                                     (revy-worker-get-dir worker)))))
        (incf revy-syncing-files)
        (message "Syncing: %s" name)
        (set-process-sentinel process
                              (lambda (process event)
                                (decf revy-syncing-files)
                                (if (string= event "finished\n")
                                    (if (< 0 revy-syncing-files)
                                        (message "Sync with %s completed [%d left]" name revy-syncing-files)
                                      (message "Syncing done"))
                                  (message "Sync with %s failed with exit code %i [%d left]"
                                           name (process-exit-status process) revy-syncing-files)))))))
  nil)

(defun revy-upload-files-sync (&rest workers)
  ("Sync files as `revy-upload-files', but blocking.
Wont return untill all workers has been synced."
  (revy-upload-files workers)
  (while (< 0 revy-syncing-files)
    (sleep 0 200))))
