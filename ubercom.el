
(defun revy-send-message (&rest message)
  "Send a message"
  (let* ((worker (if (revy-worker-p (car message))
                     (pop message)
                   revy-currrent-worker))
         (time (cond ((integerp (car message))
                      (int-to-string (pop message)))
                     ((string-equal "" (car message))
                      (pop message))
                     ((string-equal "now" (car message))
                      (pop message))
                     (t
                      "now")))

         (list (cons (revy-worker-name worker) (cons time message))))
    (mapconcat (lambda (x) (if (integerp x) (int-to-string x) x))
               list ";")
    ;;do something with the above
))



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
    (revy-shell
     (concat "scp "
             filename
             " " (revy-worker-location revy-currrent-worker)
             ":" (revy-worker-dir revy-currrent-worker)
             subdir (file-name-nondirectory filename))))

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

;; All commands are based on the current worker `revy-currrent-worker'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (revy-send-message "sketch" "PDF" "reload"))

(defun revy-pdf-goto-slide (slide)
  "Goto pdf slide."
  (revy-send-message "sketch" "PDF" "goto" slide))

(defun revy-pdf-next ()
  "Goto next slide"
  (revy-send-message "sketch" "next"))
