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
      (revy-on-worker
       'all
       (revy-shell "killall -9 worker")))))

(defun revy-open (filename &optional worker)
  "Open a new uberscript or ubertex file and start it
The file must either be an absolute path or relative to the revy-dir."
  (push (current-buffer) revy-stack)
  (find-file-other-window (revy-absolute-data-path filename))
  (when worker
    (setq revy-current-worker worker))
  (let ((extension (downcase (file-name-extension filename))))
    (cond
     ((string= extension "tex")
      (revy-manus-mode t) ;; Should this be activated?
      (revy-ubertex-mode))
     ((string= extension "song")
      (revy-ubersong-mode))
     (t
      (revy-ubersicht-mode)))))

;; TODO: make this a macro
(defmacro revy-nop (&rest _)
  "Do nothing
Ignores all arguments" nil nil)

(defun revy-end-sketch ()
  "Finish the current sketch and return to the one opening it
This function will also call revy-resync (which also aborts all)"
  (interactive)
  (revy-resync)
  (revy-return))

(defun revy-return ()
  "Returns, like finishing the current sketch like `revy-end-sketch',
but without closing it, essentially not calling revy-abort-all"
  (revy-clear-overlays)
  (pop-to-buffer (pop revy-stack))
  (let ((start (overlay-start revy-local-cursor))
        (end (overlay-end revy-local-cursor)))
    (move-overlay revy-cursor start end (current-buffer))))

(defun revy-restart ()
  "Restart the current sketch"
  (interactive)
  (let ((previous-point (point))
        (previous-cursor (overlay-start revy-local-cursor)))
    (funcall major-mode)
    (goto-char previous-cursor)
    (revy-mode-enter)
    (goto-char previous-point)))

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
  (revy-send-command "blank"))

(defun revy-blank-all ()
  "Show blank screen temporarily on all workers"
  (interactive)
  (revy-on-worker 'all
   (revy-send-command "blank")))

(defun revy-unblank-all ()
  "Continue showing what was previously shown"
  (interactive)
  (revy-on-worker 'all
   (revy-send-message "unblank")))

(defun revy-resync (&optional seed)
  "Aborts all and resyncs
Uses either the given seed or a random number between 1 and 2^32-1"
  (interactive)
  (revy-on-worker 'all
   (let* ((max-seed (- (expt 2 32) 1 1))
          (seed (or seed (+ (random max-seed) 1))))
     (revy-send-command "resync" (number-to-string seed))))
  ;; todo fix:
  (revy-kill-mplayer))

(defun revy-abort ()
  (interactive)
  (revy-send-command "abort"))

;; (defun revy-abort-all ()
;;   (interactive)
;;   (revy-on-worker 'all
;;    (revy-send-command "abort"))
;;   ;; todo fix:
;;   (revy-kill-mplayer))
(defalias 'revy-abort-all 'revy-resync)


(defun revy-calibrate (&optional worker)
  (interactive)
  (revy-on-worker (or worker 'all)
   (revy-send-lisp '(update (calibrate)))))

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
  (revy-send-lisp
   `(setq image-file ,file)
   `(setq image-position ,position)
   '(defun image-viewer ()
      (image image-file image-position))
   '(update (image-viewer))))

(defun revy-image-preload (&rest files)
  "Open one or more images, and show the first."
  (dolist (file files)
    (revy-send-message "preload" "sized" file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π PDF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defconst revy-pdf-default-position ''(sized 0.0 0.0 0.8 0.8)
  "The default postion to render pdfs")

(defun revy-pdf-open (file &optional position)
  "Open a PDF file"
  ;; (revy-send-message "start" "PDF" "sized/0,70,90p,90p" file))
  (let ((position (or position revy-pdf-default-position)))
    (revy-send-lisp
     `(setq pdf-file ,file)
     `(setq pdf-slide 0)
     `(defun pdf-slideshow ()
        ;; (pdf pdf-file pdf-slide '(centered 0.1 0.2)))
        (pdf pdf-file pdf-slide ,position))
     `(update (pdf-slideshow)))))

(defun revy-pdf-goto-slide (slide)
  "Goto pdf slide."
  ;; (revy-send-message "module" "PDF" "goto" slide))
  (revy-send-lisp `(setq pdf-slide ,slide)))

(defun revy-pdf-next ()
  "Goto next slide"
  ;; (revy-send-message "module" "next"))
  (revy-send-lisp '(setq pdf-slide (+ pdf-slide 1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Sound
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-sound (file &optional volume)
  "Play a sound overlay
Volume is integer between 0 and 128 (defaults to max)"
  ;; (revy-send-message "playsound" file))
  (revy-send-lisp `(sound ,file ,volume)))

(defun revy-stop-sounds ()
  "Stop all overlay sounds"
  (interactive)
  (revy-send-lisp '(sound-stop-all))
  ;;TODO: why do i have to do this
  (sleep-for 0 20))

(defun revy-fade-sounds (&optional duration)
  "Stop all overlay sounds"
  (interactive)
  (when (and (called-interactively-p 'any) (null duration))
    (setq duration (read-string "Duration: "))
    (when (string= duration "") (setq duration nil)))
  (if (null duration)
      (revy-send-lisp '(sound-fade-all))
    (revy-send-lisp `(sound-fade-all ,duration))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Mplayer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Todo outsource this to work through zeigen

(defvar revy-mplayer-geometry "-xy 550 -geometry 55%:45%")
(defvar revy-mplayer-flags "-vo x11")
(defconst revy-mplayer-default-flags "-nolirc -msglevel all=-1 -msglevel statusline=5 -zoom")

(defun revy-mplayer (file &optional geometry flags default-flags x y w h)
  "Play a video through mplayer on current worker"
  (revy-kill-mplayer)
  (sit-for 0.6)
  ;; -nolirc                : disable Linux Infrared Remote Control
  ;; -msglevel -all=-1      : all siclence
  ;; -msglevel statusline=5 : except statusline
  ;; -zoom                  : enable Allow software scaling
  ;; -xy <value>            : set width to value, keep aspect ratio.
  ;; -geometry <x>:<y>      : x:y position (in percent)
  ;; -xy 500 -geometry 49%:40%
  ;; -xy 620 -geometry 55%:50%
  ;; -xy 400 -geometry 55%:45%
  ;; -xy 550 -geometry 55%:45%
  ;; (revy-shell (concat "mplayer -vo x11 -nolirc -msglevel all=-1 -msglevel statusline=5 -zoom -xy 550 -geometry 55%:45% \"" file "\""))
  (let ((geometry (or geometry revy-mplayer-geometry))
        (flags (or flags revy-mplayer-flags))
        (default-flags (or default-flags revy-mplayer-default-flags))
        command)
    (setq command (concat "mplayer " default-flags " " flags " " geometry "  \"" file "\""))
    (message "%s" command)
    (revy-shell command)))

(defun revy-kill-mplayer ()
  "Killall instances of mplayer on worker"
  (interactive)
  (revy-on-worker 'all (revy-shell-sync "killall mplayer")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar revy--text-history nil
  "History variable for `revy-show-text'")

(defvar revy-text-prefix "")
(defvar revy-text-postfix "")
(defun revy-text (&optional text)
  (interactive)
  (unless text
    (setq text (read-from-minibuffer "Text: " nil nil nil 'revy--text-history)))
  (setq text (concat revy-text-prefix text revy-text-postfix))
  (revy-send-lisp `(update (text ,text))))
