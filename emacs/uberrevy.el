; This text is red!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;π Uberrevy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'revy-uberrevy)

(eval-when-compile (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Includes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'revy)
(require 'revy-uberlib "uberlib.el")
(require 'revy-uberworkers "uberworkers.el")
(require 'revy-ubermode "ubermode.el") ;revy-mode-*-function
(require 'revy-ubercom "ubercom.el") ;revy-syncing-files
(require 'revy-uberinstructions "uberinstructions.el") ;revy--show-text-history
(require 'revy-ubersicht "ubersicht.el")
(require 'revy-ubertex "ubertex.el")
(require 'revy-ubermanus "ubermanus.el") ;revy-overtex-preamble revy-overtex-postscript
(require 'revy-ubermenu "ubermenu.el") ;revy-ubermenu-commands
(require 'revy-uberprepare "uberprepare.el")


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

(defcustom revy-force-save nil
  "When non-nil, the revy-system will always save all buffers instead of prompting.
Default is nil, as is the default behaviour of Emacs.
Revy-simple sets this to t."
  :group 'uberrevy)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Internal variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar revy-stack '()
  "Variable storing the stack of sketches being executed")

(defvar revy-dir nil
  "The directory the current revy is stored in")

(defvar revy-file nil
  "The current .revy file")

(defvar revy-buffer nil
  "The Buffer for the current .revy file")

(defvar revy-workers nil
  "All workers in the revy")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Workers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remember to recreate all workers when any of these are changed
(defvar revy-worker-default-port "9999"
  "The default port for new workers")

(defvar revy-worker-default-display ":0"
  "The default display for new workers")

(defvar revy-worker-default-dir (file-name-as-directory (concat "~/revy" (format-time-string "%Y")))
  "The default directory to store files for the revy on new workers")

(defvar revy-worker-default-installation "~/ubertex/"
  "The default directory to store files for the revy on new workers")

(defvar revy-current-worker nil
  "The current worker.
Use `setq-default' to set the default worker when none is chosen explicitly.")
(make-variable-buffer-local 'revy-current-worker)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Faces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defface revy-cursor-face
  '((((type x w32 mac))
     (:foreground "black" :background "firebrick4")))
  "")

(defface revy-local-cursor-face
  '((((type x w32 mac))
     ;; (:foreground "black" :background "4D3B3B")))
     (:foreground "black" :background "gray20")))
  "")

(defface revy-hidden-face
  '((((type x w32 mac))
     (:font "DejaVu Sans Mono" :height 50 :foreground "gray50")))
  "")

(setq revy-cursor (make-overlay 0 20 (current-buffer) t nil))
(overlay-put revy-cursor 'revy t)
(overlay-put revy-cursor 'priority 5000)
(overlay-put revy-cursor 'face 'revy-cursor-face)



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
  "Clear all revy overlays in buffer
The overlays are not deleted, so if they are referenced like revy-cursor, they
can still be used when moved in to the buffer. So the enter functions will still
work with the global cursor"
  (let ((beginning (buffer-end -1))
        (end (buffer-end 1)))
    (remove-overlays beginning end 'revy t)
    (remove-overlays beginning end 'face 'revy-cursor-face)
    (remove-overlays beginning end 'face 'revy-local-cursor-face)
    (remove-overlays beginning end 'face 'revy-hidden-face)))
