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
