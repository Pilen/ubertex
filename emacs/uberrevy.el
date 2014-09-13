;thisisred

(provide 'uberrevy)

(eval-when-compile (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgroup uberrevy nil
  "Group for all uberrevy settings")

(defcustom revy-scp-mode nil
  "When true, will scp the .pdf to the server when starting/restarting"
  :group 'uberrevy)

(defcustom revy-follow-cursor 'recenter
  "How point should follow the cursor.
nil means nothing is done.
'follow means the point will follow the cursor when it is moved.
'recenter means point should follow and that the window should recenter when the cursor is moved (This is the default)."
  :group 'uberrevy
  :type '(choice (const :tag "nothing" nil)
                 (const :tag "Follow cursor" follow)
                 (const :tag "Follow and recenter" recenter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Internal variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar revy-leiter nil "Variable storing the leiter process")
(defvar revy-stack '() "Variable storing the stack of sketches being executed")
(defvar revy-dir nil
  "The directory the current revy is stored in")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Workers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar revy-worker-default-dir (concat "~/revy" (format-time-string "%Y"))
  "The default directory to store files for the revy on workers")

(defstruct revy-worker name port location display dir) ; user@location:0.display

(defvar revy-current-worker nil
  "The current worker.
Use `setq-default' to set the default worker when none is chosen explicitly.")
(make-variable-buffer-local 'revy-current-worker)

(defconst revy-worker-all (make-revy-worker :name "all"
                                            :port ""
                                            :location ""
                                            :display ""
                                            :dir ""))

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

(defvar revy-local-cursor nil
  "Local cursors show where you are in the current buffer.
This might not be the current cursor being displayed as buffers
can be temporarily pushed on the `revy-stack' while another is executed")
(make-variable-buffer-local 'revy-local-cursor)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Misc functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-clear-overlays ()
  "Delete all revy overlays in buffer"
  (let ((beginning (buffer-end -1))
        (end (buffer-end 1)))
    (remove-overlays beginning end 'revy t)
    (remove-overlays beginning end 'face 'revy-cursor-face)
    (remove-overlays beginning end 'face 'revy-local-cursor-face)
    (remove-overlays beginning end 'face 'revy-hidden-face)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Helper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-string-ends-with (string regex)
  "Returns true if the string ends with the prefix specified in regex."
  (and (string-match (concat regex "^") string)
       t))

(defun revy-string-starts-with (string regex)
  "Returns true if the string starts with the prefix specified in regex."
  (and (string-match (concat "^" regex) string)
       t))

(defun revy-data-path (file &optional alternative-extension)
  "Returns the path for the file as a relative path in the revy-dir."
  (when (null alternative-extension)
    (setq alternative-extension ""))
  ;; Replace file extension.
  (setq file (concat (file-name-sans-extension file)
                     (if (revy-string-starts-with alternative-extension "\\.") "" ".")
                     alternative-extension))

  (if (file-name-absolute-p file)
      (if (revy-string-starts-with file (file-name-as-directory revy-dir))
            (substring file (match-end 0))
          (error "File is located outside the revy-dir!"))
    ;; A relative path should simply stay relative.
    file))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Includes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'ubercom)
;; (require 'ubertex)
;; (require 'ubersicht)
;; (require 'manus)
