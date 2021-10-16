;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
                                        ;π Ubercom
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Handle communication
(provide 'revy-ubercom)

(defconst revy-header-size 512
  "The fixed size of headers for zeigen")

(defun revy-send-lisp (&rest expressions)
  "Execute the expressions on the current worker."
  (let* ((lisp (mapconcat 'prin1-to-string expressions " "))
         (size (int-to-string (string-bytes lisp)))
         (time (number-to-string (revy--target-tick)))
         (header (format "%s;%s;lisp;%s" revy-current-worker time size))
         (padding-size (- revy-header-size (string-bytes header)))
         (padding (if (< padding-size 0)
                      (error "Header too big")
                    (make-string padding-size 0)))
         (message (concat header padding lisp)))
    (message "%s" lisp)
    (revy--send-message message)))

(defun revy-send-command (command &optional options)
  "Send the commands to the current worker.
To send lisp code use `revy-send-lisp' instead."
  (setq options (or options ""))
  (let* ((time "0")
         ;; (time (number-to-string (revy--target-tick))) ;; Not yet implemented
         (options (or options ""))
         (header (format "%s;%s;%s;%s" revy-current-worker time command options))
         (padding-size (- revy-header-size (string-bytes header)))
         (padding (if (< padding-size 0)
                      (error "Header too big")
                    (make-string padding-size 0)))
         (message (concat header padding)))
    (revy--send-message message)))

(defun revy--send-message (message)
  "Send a message to the current worker"
  (let ((workers (revy-get-workers revy-current-worker)))
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

(defun revy--target-tick ()
  "Calculate the next target frame"
  (if (cdr (revy-get-workers revy-current-worker))
      (let* ((diff (time-subtract (current-time) revy-previous-resync))
             (high (pop diff))
             (seconds (pop diff))
             (micro (pop diff)))
        (+ (* high (expt 2 16) 1000)
           (* seconds 1000)
           (/ micro 1000)
           revy-headway))
    0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;π Simple work
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Should concat by itself
(defun revy-shell (command)
  "Evaluate shell command on the current worker asynchronously."
  ;; Save excursion to avoid the output buffer coming up
  (save-window-excursion
    (dolist (worker (revy-get-workers revy-current-worker))
      (start-process "revy-shell" "*revy-shell*"
                     "ssh" (concat (aref worker revy-worker-user-index) "@" (aref worker revy-worker-location-index))
                     (concat "export DISPLAY=" (aref worker revy-worker-display-index) ";\n"
                             "cd " (aref worker revy-worker-dir-index) ";\n"
                             command)))))

(defun revy-shell-sync (command)
  ;; Todo, does not seem to work with current worker?!
  "Evaluate shell command on the current worker synchronously."
  (save-window-excursion
    ;; Unlike `start-process', `call-process' inserts at point in buffer, not at end
    (with-current-buffer (get-buffer-create "*revy-shell*")
      (goto-char (point-max))
      (dolist (worker (revy-get-workers revy-current-worker))
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
  ;; Todo fix name of completing syncing machine
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
                                           (file-name-as-directory (expand-file-name revy-dir))
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
                                    (progn
                                      (message "Syncing done")
                                      ;; Ensure workers reload files
                                      (revy-on-worker
                                       'all
                                       (revy-send-command "flush_entire_cache"))
                                      ))
                                (message "Sync with %s failed with exit code %i [%d left]"
                                         name (process-exit-status process) revy--uploading-files))))))
  nil)

(defun revy-upload-files-sync (&rest workers)
  ("Sync files as `revy-upload-files', but blocking.
Wont return untill all workers has been synced."
   (revy-upload-files workers)
   (while (> revy--uploading-files 0)
     (sleep 0 200))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
                                        ;π Starting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-start-workers ()
  "Start all the workers not already online"
  (dolist (worker (revy-get-workers 'all))
    (with-current-buffer (get-buffer-create "*revy-worker-start*")
      (erase-buffer)
      (let ((channel (condition-case nil
                         (open-network-stream
                          "revy-worker-start"
                          (current-buffer)
                          (aref worker revy-worker-location-index)
                          (aref worker revy-worker-port-index))
                       (error nil))))
        (if channel
            (progn
              (process-send-string channel (concat  "nil;0;ping;;" (make-string 500 0))) ;; TODO: make command correct / fix padding
              (accept-process-output channel 1)
              (goto-char (point-min))
              (if (search-forward "Got it. Bye" nil t)
                  (progn (message "%s already up" (aref worker revy-worker-location-index)) t)
                (error "Process returned nonsens. Is the host/port correct and the software updated?")))
          ;; Is there a connection to the host?
          (if (= 0 (revy-shell-local-sync (concat "ping -c 1 -W 1 " (aref worker revy-worker-location-index))))
              (progn ;; (revy-shell-sync (message (concat "cd " (aref worker revy-worker-installation-index) "zeigen2/src/" "; "
                ;;                                   "./repl -b -w -d " (aref worker revy-worker-dir-index))))
                ;; We want to run this command on the worker, and for revy-shell-sync we need the worker name which is not stored in the worker structure
                (if (not (eq 0 (call-process "ssh" nil "*revy-shell*" t
                                             (concat (aref worker revy-worker-user-index) "@" (aref worker revy-worker-location-index))
                                             (concat "export DISPLAY=" (aref worker revy-worker-display-index) ";\n"
                                                     "cd " (aref worker revy-worker-installation-index) "worker/" "; "
                                                     "./worker -b -w -d " (aref worker revy-worker-dir-index)))))
                    (error "Could not start worker %s, but host should be up." (aref worker revy-worker-location-index))
                  (message "Starting %s" (aref worker revy-worker-location-index))
                  t))
            (error "No connection to host %s" (aref worker revy-worker-location-index)))))))
  (message "Starting done")
  t)

(defun revy-update-ubertex-on-workers ()
  "Update the program on all the workers"
  (interactive)
  (dolist (worker (revy-get-workers 'all))
    (with-current-buffer (get-buffer-create "*revy-update*") (erase-buffer))
    (let ((target (concat (aref worker revy-worker-user-index) "@" (aref worker revy-worker-location-index)))
          (command (concat
                    "killall -q -9 worker;"
                    "cd " (aref worker revy-worker-installation-index) ";"
                    "cd worker;"
                    "git pull;"
                    "LANG=en_US.UTF-8 make"
                    )))
      (message command)
      (start-process "revy-update" "*revy-update*"
                     "ssh" target command))))

;; (defun revy-update-software ()
;;   "Update the software on all workers and the current machine"
;;   (interactive)
;;   (revy-shell-local-sync (concat "cd " revy-ubertex-dir ";"
;;                                  "git pull") )
;;   (dolist (worker (revy-get-workers 'all))
;;     (revy-shell-sync (concat "cd " (aref worker revy-worker-installation-index) "zeigen2/src/" "; "
;;                              ;; Not handling hwclock out of date
;;                              "git pull ")
;;                      )))
