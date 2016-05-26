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

  ;; Start any workers not online
  (revy-start-workers))

(defun revy-quit ()
  "Quit all workers"
  (interactive)
  (when (yes-or-no-p "Quit the revy closing all workers?")
    (when (yes-or-no-p "Are you sure you want to close all workers?")
      (revy-shell "killall -9 repl" 'all))))

(defun revy-open (filename &optional worker)
  "Open a new uberscript or ubertex file and start it
The file must either be an absolute path or relative to the revy-dir."
  (push (current-buffer) revy-stack)
  (find-file-other-window (revy-absolute-data-path filename))
  (when worker
    (setq revy-current-worker worker))
  (if (string= (downcase (file-name-extension filename)) "tex")
      (progn (revy-manus-mode t) ;; Should this be activated?
             (revy-ubertex-mode))
    (revy-ubersicht-mode)))

;; TODO: make this a macro
(defun revy-nop (&optional &rest _)
  "Do nothing
Ignores all arguments")

(defun revy-end-sketch ()
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
  (revy-send-command nil "blank"))

(defun revy-blank-all ()
  "Show blank screen temporarily on all workers"
  (interactive)
  (revy-send-command 'all "blank"))

(defun revy-unblank-all ()
  "Continue showing what was previously shown"
  (interactive)
  (revy-send-message "unblank"))

(defun revy-abort ()
  (interactive)
  (revy-send-command nil "abort"))

(defun revy-abort-all ()
  (interactive)
  (revy-send-command 'all "abort")
  ;; todo fix:
  (revy-kill-mplayer))

(defun revy-kill (&optional sketch)
  (interactive)
  (if (null sketch)
      (revy-send-message "kill")
    (revy-send-message "kill" sketch)))

(defun revy-test-all ()
  (interactive)
  (revy-send-message revy-worker-all "start" "Test"))

(defun revy-calibrate ()
  (interactive)
  (revy-send-lisp 'all '(next-update 'calibrate)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Image
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst revy-image-default-position ''(full)
       "The default postion to render images")

(defun revy-image (file &optional position)
  "Open one or more images, and show the first."
  ;; (unless dimensions
  ;;   (setq dimensions "sized"))
  ;; (revy-send-message "start" "Image" dimensions file))
  ;; (setq position (or position ''(sized 0.0 0.0 1.0 1.0)))
  (setq position (or position revy-image-default-position))
  (revy-send-lisp nil
                  `(setq image-file ,file)
                  `(setq image-position ,position)
                  '(defun image-viewer ()
                     (image image-file image-position))
                  '(next-update 'image-viewer)))

(defun revy-image-preload (&rest files)
  "Open one or more images, and show the first."
  (dolist (file files)
    (revy-send-message "preload" "sized" file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π PDF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-pdf-open (file)
  "Open a PDF file"
  ;; (revy-send-message "start" "PDF" "sized/0,70,90p,90p" file))
  (revy-send-lisp nil
                  `(setq pdf-file ,file)
                  '(setq pdf-slide 0)
                  '(defun pdf-slideshow ()
                     ;; (pdf pdf-file pdf-slide '(centered 0.1 0.2)))
                     (pdf pdf-file pdf-slide '(sized 0.0 0.0 0.8 0.8)))
                  `(next-update 'pdf-slideshow)))

(defun revy-pdf-reload ()
  "Reload current pdf"
  (revy-send-message "module" "PDF" "reload"))

(defun revy-pdf-goto-slide (slide)
  "Goto pdf slide."
  ;; (revy-send-message "module" "PDF" "goto" slide))
  (revy-send-lisp nil `(setq pdf-slide ,slide)))

(defun revy-pdf-next ()
  "Goto next slide"
  ;; (revy-send-message "module" "next"))
  (revy-send-lisp nil '(setq pdf-slide (+ pdf-slide 1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Sound
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-sound (file &optional volume)
  "Play a sound overlay"
  ;; (revy-send-message "playsound" file))
  (revy-send-lisp nil `(sound ,file ,volume)))

(defun revy-stop-sounds ()
  "Stop all overlay sounds"
  (interactive)
  (revy-send-lisp nil '(sound-stop-all))
  ;;TODO: why do i have to do this
  (sleep-for 0 20))

(defun revy-fade-sounds (&optional duration)
  "Stop all overlay sounds"
  (interactive)
  (when (and (called-interactively-p 'any) (null duration))
    (setq duration (read-string "Duration: "))
    (when (string= duration "") (setq duration nil)))
  (if (null duration)
      (revy-send-lisp nil '(sound-fade-all))
    (revy-send-lisp nil `(sound-fade-all ,duration))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Mplayer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Todo outsource this to work through zeigen

(defun revy-mplayer (file &optional x y w h)
  "Play a video through mplayer on current worker"
  (revy-kill-mplayer)
  ;; (revy-shell (concat "mplayer -nolirc -msglevel all=-1 -msglevel statusline=5 -xy 500 -geometry 49%:40% \"" file "\"")))
  ;; (revy-shell (concat "mplayer -vo x11 -nolirc -msglevel all=-1 -msglevel statusline=5 -xy 400 -geometry 55%:45% \"" file "\"")))
  (revy-shell (concat "mplayer -vo x11 -nolirc -msglevel all=-1 -msglevel statusline=5 -zoom -xy 620 -geometry 55%:50% \"" file "\"")))

(defun revy-kill-mplayer ()
  "Killall instances of mplayer on worker"
  (interactive)
  (revy-shell-sync "killall mplayer"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar revy--show-text-history nil
  "History variable for `revy-show-text'")

(defun revy-text (&optional text)
  (interactive)
  (unless text
    (setq text (read-from-minibuffer "Text: " nil nil nil 'revy--show-text-history)))
  (revy-send-lisp nil `(next-update 'text ,text)))
