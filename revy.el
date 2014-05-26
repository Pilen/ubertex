
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Workers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar revy-default-dir (concat "~/revy" (format-time-string "%Y")))


(defstruct revy-worker name port location display dir) ; user@location:0.display

(defvar revy-worker-brok (make-revy-worker :name "brok" :port "9999" :location "revy@brok" :display ":0" :dir revy-default-dir))
(defvar revy-worker-local (make-revy-worker :name "local" :port "9999" :location "localhost" :display ":0" :dir revy-default-dir))
(defvar revy-default-worker revy-worker-brok)
(make-variable-buffer-local 'revy-default-worker)
(defvar revy-currrent-worker revy-default-worker)
(make-variable-buffer-local revy-currrent-worker)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface revy-cursor-face
  '((((type x w32 mac))
     (:foreground "black" :background "red")))
  "")

(defface revy-local-cursor-face
  '((((type x w32 mac))
     (:foreground "black" :background "4D3B3B")))
  "")

(defface revy-hidden-face
  '((((type x w32 mac))
     (:font "DejaVu Sans Mono")
     (:height 90)
     (:background "gray20")))
  "")

;; TODO: Clean up the below
(set-face-attribute 'revy-hidden-face nil :height 50)
(set-face-attribute 'revy-hidden-face nil :font "DejaVu Sans Mono")

(set-face-background 'revy-hidden-face "gray30")
(set-face-foreground 'revy-hidden-face "gray50")
(set-face-background 'revy-cursor-face "firebrick4")

(setq revy-cursor (make-overlay 0 10 (current-buffer) t t))
(overlay-put revy-cursor 'face 'revy-cursor-face)
(overlay-put revy-cursor 'priority 5000)
(overlay-put revy-cursor 'revy t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Cursors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar revy-cursor nil
  "The global cursor is where you actually are.
This is what currently is being displayed")

;; Find out what to do here, perhaps this should be buffer local, but how do you then see which cursor is actually being shown?. Perhaps this should be buffer local in ubersicht but not ubertex? Perhaps we should have both a buffer cursor and a global cursor being on top.
(defvar revy-local-cursor nil
  "Local cursors show where you are in the current buffer.
This might not be the current cursor being displayed as buffers
can be temporarily pushed on the `revy-stack' while another is executed")
(make-variable-buffer-local 'revy-local-cursor)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Stack
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar revy-stack '())



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Includes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ubercom)
(require 'ubertex)
(require 'ubersicht)
