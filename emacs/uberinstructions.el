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
  (setq revy-stack '())
  (push (current-buffer) revy-stack)
  ;; Ensure we are in correct mode, most likely unnecessary most of the time
  ;; (revy-ubersicht-mode)
  ;; It is!
  )

(defun revy-open (filename &optional screen)
  "Open a new uberscript or ubertex file and start it"
  (push (current-buffer) revy-stack)
  (find-file-other-window filename)
  (when screen
    (setq revy-default-screen screen))
  (if (string= (downcase (file-name-extension filename)) "tex")
      (progn (revy-manus-mode t) ;; Should this be activated?
             (revy-ubertex-mode))
    (revy-ubersicht-mode)))

(defun revy-nop (&optional &rest _)
  "Do nothing
Ignores all arguments")

(defun revy-quit ()
  "Finish the current sketch and return to the one opening it
This function will also call revy-abort-all "
  (interactive)
  (revy-clear-overlays)
  (revy-abort-all)
  (pop-to-buffer (pop revy-stack))
  (let ((start (overlay-start revy-local-cursor))
        (end (overlay-end revy-local-cursor)))
    (move-overlay revy-cursor start end (current-buffer))))

(defun revy-restart ()
  "Restart the current sketch"
  (interactive)
  (funcall major-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;π Foreign worker
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All the following instructions are based on the current worker
;; aka the `revy-current-worker'
;; (use revy-on-worker to run the instruction on another worker instead).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-blank ()
  "Show blank screen temporarily"
  (interactive)
  (revy-send-message "blank"))

(defun revy-blank-all ()
  "Show blank screen temporarily on all workers"
  (revy-blank))

(defun revy-unblank-all ()
  "Continue showing what was previously shown"
  (interactive)
  (revy-send-message "unblank"))

(defun revy-abort ()
  (interactive)
  (revy-send-message "abort"))

(defun revy-abort-all ()
  (interactive)
  (revy-send-message revy-worker-all "abort")
  ;; todo fix:
  (revy-kill-mplayer))

(defun revy-kill (&optional sketch)
  (interactive)
  (if (null sketch)
      (revy-send-message "kill")
    (revy-send-message "kill" sketch)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Image
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-image-open (file &optional dimensions)
  "Open one or more images, and show the first."
  (unless dimensions
    (setq dimensions "sized"))
  (revy-send-message "start" "Image" dimensions file))

(defun revy-image-preload (&rest files)
  "Open one or more images, and show the first."
  (dolist (file files)
    (revy-send-message "preload" "sized" file)))

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
;π Mplayer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Todo outsource this to work through zeigen

(defun revy-mplayer (file &optional x y w h)
  "Play a video through mplayer on current worker"
  (revy-kill-mplayer)
  ;; (revy-shell (concat "mplayer -nolirc -msglevel all=-1 -msglevel statusline=5 -xy 500 -geometry 49%:40% \"" file "\"")))
  (revy-shell (concat "mplayer -nolirc -msglevel all=-1 -msglevel statusline=5 -xy 400 -geometry 55%:45% \"" file "\"")))

(defun revy-kill-mplayer ()
  "Killall instances of mplayer on worker"
  (interactive)
  (revy-shell-sync "killall mplayer"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-show-text (text)
  (interactive "sText: ")
  (revy-send-message "start" "Text" "text" text))
