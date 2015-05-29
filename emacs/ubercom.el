;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;π Ubercom
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handle communication
(provide 'revy-ubercom)

(defconst revy-header-size 512
  "The fixed size of headers for zeigen")

(defun revy-send-lisp (worker &rest expressions)
  "Execute the expressions on the current worker."
  (setq worker (or worker revy-current-worker))
  (let* ((lisp (mapconcat 'prin1-to-string expressions " "))
         (size (int-to-string (string-bytes lisp)))
         (time "0")
         (header (format "%s;%s;lisp;%s" worker time size))
         (padding-size (- revy-header-size (string-bytes header)))
         (padding (if (< padding-size 0)
                      (error "Header too big")
                    (make-string padding-size 0)))
         (message (concat header padding lisp)))
    (message "%s" lisp)
    (revy--send-message worker message)))

(defun revy-send-command (worker command &optional options)
  "Send the commands to the current worker.
To send lisp code use `revy-send-lisp' instead."
  (setq options (or options ""))
  (setq worker (or worker revy-current-worker))
  (let* ((time "0")
         (options (or options ""))
         (header (format "%s;%s;%s;%s" worker time command options))
         (padding-size (- revy-header-size (string-bytes header)))
         (padding (if (< padding-size 0)
                      (error "Header too big")
                    (make-string padding-size 0)))
         (message (concat header padding)))
    (revy--send-message worker message)))

(defun revy--send-message (worker message)
  "Send a message to the current worker"
  (let ((workers (revy-get-workers worker)))
    (dolist (worker workers)
      (let ((channel (aref worker revy-worker-channel-index)))
        (unless (and (process-live-p channel) nil)
          ;; TODO: Try to start server on failure to connect
          ;; TODO: Better error logging
          (setq channel (open-network-stream "revy-worker-channel" nil
                                             (aref worker revy-worker-location-index)
                                             (aref worker revy-worker-port-index)))
          (aset worker revy-worker-channel-index channel))
        (with-demoted-errors "Error could not send message to worker: %S"
          (process-send-string channel message))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Simple work
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Should concat by itself
(defun revy-shell (command &optional worker)
  "Evaluate shell command on a given worker asynchronously.
If no worker is given/worker is nil,
the command will be executed on revy-current-worker."
  ;; Save excursion to avoid the output buffer coming up
  (save-window-excursion
    (when (null worker)
      (setq worker revy-current-worker))
    (dolist (worker (revy-get-workers worker))
      (start-process "revy-shell" "*revy-shell*"
                     "ssh" (concat (aref worker revy-worker-user-index) "@" (aref worker revy-worker-location-index))
                     (concat "export DISPLAY=" (aref worker revy-worker-display-index) ";\n"
                             "cd " (aref worker revy-worker-dir-index) ";\n"
                             command)))))

(defun revy-shell-sync (command &optional worker)
  ;; Todo, does not seem to work with current worker?!
  "Evaluate shell command on a given worker synchronously.
If no worker is given/worker is nil,
the command will be executed on revy-current-worker."
  (save-window-excursion
    (when (null worker)
      (setq worker revy-current-worker))
    ;; Unlike `start-process', `call-process' inserts at point in buffer, not at end
    (with-current-buffer (get-buffer-create "*revy-shell*")
      (goto-char (point-max))
      (dolist (worker (revy-get-workers worker))
        (call-process "ssh" nil "*revy-shell*" t
                      (concat (aref worker revy-worker-user-index) "@" (aref worker revy-worker-location-index))
                      (concat "export DISPLAY=" (aref worker revy-worker-display-index) ";\n"
                            "cd " (aref worker revy-worker-dir-index) ";\n"
                            command))))))

(defun revy-shell-local (command)
  "Evaluate shell command on this machine asynchronously."
  (save-window-excursion
    (start-process-shell-command "revy-shell" "*revy-shell*" command)))

(defun revy-shell-local-sync (command)
  "Evaluate shell command on this machine synchronously."
  (save-window-excursion
    (with-current-buffer (get-buffer-create "*revy-shell*")
      (goto-char (point-max))
      (call-process-shell-command command nil "*revy-shell*" t))))

;; Might be unnecessary if using sshmount
(defun revy-scp-file (filename subdir)
  "Copy a file to a worker using scp"
  (when revy-scp-mode
    (dolist (worker (revy-get-workers revy-current-worker))
      (revy-shell
       (concat "scp "
               filename
               " " (concat (aref worker revy-worker-user-index) "@" (aref worker revy-worker-location-index))
               ":" (aref worker revy-worker-dir-index)
               (file-name-as-directory subdir) (file-name-nondirectory filename))))))

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

(defvar revy--uploading-files 0
  "Number off machines being synced to currently")

(defun revy-upload-files (&optional worker)
  ;; Todo fix dokumentation
  "Upload files to workers.
If no workers are specified, the files will be uploaded to all workers.
Remember only nonvirtual workers are updated

Uses rsync to upload the files, based on the timestamp"
  (interactive)
  ;; TODO: if rsync fails we might want to use scp (check exit code).

  (when (> revy--uploading-files 0)
    (error "Syncing already in progress"))

  ;; (revy-send-message "syncfiles")
  (unless worker
    (setq worker 'all))

  (dolist (worker_v (revy-get-workers worker))
    (lexical-let* ((name worker)
                   ;; TODO: name is not unique and is thus not the best indicator, not an error though
                   (process (start-process (format "revy-rsync-%s" name)
                                           (format "*revy-rsync-%s*" name)
                                           "rsync"
                                           "-r" "-u" "-t" "-P" "-e" "ssh"
                                           (file-name-as-directory revy-dir)
                                           (concat (concat (aref worker_v revy-worker-user-index) "@" (aref worker_v revy-worker-location-index))
                                                   ":"
                                                   (aref worker_v revy-worker-dir-index)))))
      (incf revy--uploading-files)
      (message "Syncing: %s" name)
      (set-process-sentinel process
                            (lambda (process event)
                              (decf revy--uploading-files)
                              (if (string= event "finished\n")
                                  (if (> revy--uploading-files 0)
                                      (message "Sync with %s completed [%d left]" name revy--uploading-files)
                                    (message "Syncing done"))
                                (message "Sync with %s failed with exit code %i [%d left]"
                                         name (process-exit-status process) revy--uploading-files))))))
  nil)

(defun revy-upload-files-sync (&rest workers)
  ("Sync files as `revy-upload-files', but blocking.
Wont return untill all workers has been synced."
  (revy-upload-files workers)
  (while (> revy--uploading-files 0)
    (sleep 0 200))))
