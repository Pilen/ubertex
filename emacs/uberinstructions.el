;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;π Uberinstructions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'revy-uberinstructions)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;π Local
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; uberscript handling
;; These functions do stuff on the controller, not the workers

(defun revy-start ()
  "Start a loaded revy"
  (interactive)
  (setq revy-stack '())
  (push (current-buffer) revy-stack)
  ;; Ensure we are in correct mode, most likely unnecessary most of the time
  ;; (revy-ubersicht-mode)
  ;; It is!
  )

(defun revy-open (filename &optional screen)
  "Open a new uberscript or ubertex file and start it"
  (message "running revy-open!")
  (push (current-buffer) revy-stack)
  (find-file-other-window filename)
  (when screen
    (setq revy-default-screen screen))
  (if (string= (downcase (file-name-extension filename)) "tex")
      (progn (revy-manus-mode t) ;; Should this be activated?
             (revy-ubertex-mode t))
    (revy-ubersicht-mode)))

(defun revy-nop (&optional &rest _)
  "Do nothing
Ignores all arguments")

(defun revy-quit ()
  "Finish the current sketch and return to the one opening it
This function will also call revy-abort-all "
  (interactive)
  (revy-unhide)
  (revy-abort-all)
  (pop-to-buffer (pop revy-stack)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;π Foreign worker
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All the following instructions are based on the current worker
;; aka the `revy-current-worker'

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-blank ()
  (interactive)
  (revy-send-message "blank"))

(defun revy-blank-all ()
  (revy-blank))

(defun revy-unblank-all ()
  (interactive)
  (revy-send-message "unblank"))

(defun revy-abort ()
  (interactive)
  (revy-send-message "abort"))

(defun revy-abort-all ()
  (interactive)
  (revy-send-message revy-worker-all "abort"))

(defun revy-kill (&optional sketch)
  (interactive)
  (if (null sketch)
      (revy-send-message "kill")
    (revy-send-message "kill" sketch)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Image
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-image-open (&rest files)
  "Open one or more images, and show the first."
  (revy-send-message "start" "Image" "sized" (pop files))
  (mapc (lambda (file) (revy-send-message "preload" file)) files))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π PDF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-pdf-open (file)
  "Open a PDF file"
  (revy-send-message "start" "PDF" "sized/0,70,90p,90p" file))

(defun revy-pdf-reload ()
  "Reload current pdf"
  (revy-send-message "module" "PDF" "reload"))

(defun revy-pdf-goto-slide (slide)
  "Goto pdf slide."
  (revy-send-message "module" "PDF" "goto" slide))

(defun revy-pdf-next ()
  "Goto next slide"
  (revy-send-message "module" "next"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Sound
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-play-sound (file)
  "Play a sound overlay"
  (revy-send-message "playsound" file))

(defun revy-stop-sounds ()
  "Stop all overlay sounds"
  (interactive)
  (revy-send-message "stopsounds"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-show-text (text)
  (interactive "sText: ")
  (revy-send-message "start" "Text" "text" text))
