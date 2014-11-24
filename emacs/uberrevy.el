; This text is red!

(provide 'revy-uberrevy)

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

(defvar revy-workers nil
  "All workers in the revy")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Workers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-create-worker (name &optional location port display dir installation)
  "Create a worker
`Name' is a string with the name of the worker.
`location' is the location on the network including username and IP or hostname eg. revy@192.168.0.123 or revy@brok.
`port' is the listening port of zeigen on the worker.
`display' is the display the worker should display at, most likely :0 if it has only one display.
`dir' is the directory on the worker where all files for the revy are stored.

The worker and its informations will actually be stored in the internal list, but must be accessed through the returned value"

  ;; It can  be accessed through the variable created revy-worker-get-name (where name is the specified value)"

  ;; format of workers:
  ;; [revy-worker name location port display dir installation]
  ;;
  ;; Where revy-worker is a constant atom and the others are variables
  ;; If the location is nil it is considered virtual
  ;; an alias for an existing machine

  (when (integerp port)
    (setq port (int-to-string port)))

  (unless (null dir)
    (setq dir (file-name-as-directory dir)))

  (let* ((symbol (intern name))
         (worker (assq symbol revy-workers))
         (insert nil))
    (if worker
        ;; A worker exists, so update that instead
        (setq worker (cdr worker))
      ;; A new worker has to be created
      (setq worker (make-vector 7 nil))
      (setq insert t))
    ;; Insert data
    (aset worker 0 'revy-worker)
    (aset worker 1 name)
    (aset worker 2 location)
    (aset worker 3 port)
    (aset worker 4 display)
    (aset worker 5 dir)
    (aset worker 6 installation)

    ;; Add to list
    (when insert
      (if revy-workers
          ;; Store workers in the back
          ;; It is assumed that the most important workers are created first
          (nconc revy-workers (list (cons symbol worker)))
        ;; Cant use nconc on nil
        (setq revy-workers (list (cons symbol worker)))))

    ;; ;; Create variable for simple access to the worker list
    ;; (set (intern (concat "revy-worker-" name)) (vector 'revy-worker name)))))
    (vector 'revy-worker symbol)))

(defun revy-workerp (object)
  "Return t if OBJECT is a revy-worker
It does NOT ensure the worker is actually defined in the list of revy-workers"
  (and (vectorp object) (= (length object) 2) (eq (aref object 0) 'revy-worker) t))

(defun revy-worker--get-property (worker i)
  "Returns element i from worker in the list of revy-workers"
  (unless (revy-workerp worker)
    (error "Not a worker: %s" worker))
  (let ((actual-worker (assq (aref worker 1) revy-workers)))
    (unless actual-worker
      (error "No such worker is defined: %s" (aref worker 1)))
    (aref (cdr actual-worker) i)))

(defun revy-worker-get-name (worker)
  "Returns name of worker."
  (revy-worker--get-property worker 1))

(defun revy-worker-get-location (worker)
  "Returns location of worker."
  (revy-worker--get-property worker 2))

(defun revy-worker-get-port (worker)
  "Returns port of worker."
  (revy-worker--get-property worker 3))

(defun revy-worker-get-display (worker)
  "Returns display of worker."
  (revy-worker--get-property worker 4))

(defun revy-worker-get-dir (worker)
  "Returns dir of worker."
  (revy-worker--get-property worker 5))

(defun revy-worker-installation (worker)
  "Returns installation of worker."
  (revy-worker--get-property worker 6))

(defun revy-worker-get-all-workers ()
  "Returns all workers"
  (mapcar
   (lambda (worker)
     (vector 'revy-worker (car worker)))
   revy-workers))



(defvar revy-worker-default-dir (file-name-as-directory (concat "~/revy" (format-time-string "%Y")))
  "The default directory to store files for the revy on workers")

(defvar revy-worker-default-installation (file-name-as-directory (concat "~/revy" (format-time-string "%Y")))
  "The default directory to store files for the revy on workers")

(defvar revy-current-worker nil
  "The current worker.
Use `setq-default' to set the default worker when none is chosen explicitly.")
(make-variable-buffer-local 'revy-current-worker)

(defconst revy-worker-all (revy-create-worker "all")
  "Default virtual worker for all workers.")

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

(setq revy-cursor (make-overlay 0 20 (current-buffer) t t))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Helper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-string-ends-with (string regex)
  "Returns true if the string ends with the prefix specified in regex."
  (and (string-match (concat regex "$") string)
       t))

(defun revy-string-starts-with (string regex)
  "Returns true if the string starts with the prefix specified in regex."
  (and (string-match (concat "^" regex) string)
       t))

(defun revy-replace-extension (file extension)
  "Replace extension of FILE with EXTENSION
If FILE has no extension, EXTENSION is simply added at the end."
  (concat (file-name-sans-extension file)
          (if (revy-string-starts-with extension "\\.") "" ".")
          extension))

(defun revy-data-path (file &optional alternative-extension)
  "Returns the path for the file as a relative path in the revy-dir."

  ;; Replace file extension.
  (when alternative-extension
    (setq file (revy-replace-extension file alternative-extension)))

  (if (file-name-absolute-p file)
      (if (revy-string-starts-with file (file-name-as-directory revy-dir))
            (substring file (match-end 0))
          ;; (error "File is located outside the revy-dir"))
          (message "File is located outside the revy-dir!"))
    ;; A relative path should simply stay relative.
    file))


(defun revy-define-buffer-key (key command)
  "Define a keyboard shortcut for the majormode locally in the current buffer"
  (use-local-map (copy-keymap (current-local-map)))
  (local-set-key key command))

(defun revy-on-worker (worker function &rest args)
  "Run instruction on another worker than the current
Used like (revy-on-worker revy-worker-brok 'revy-play-sound \"sound.mp3\")"
  (let ((revy-current-worker worker))
    (apply function args)))


(defun goto-match-paren ()
  "Go to the matching parenthesis if on parenthesis. Else go to the
   opening parenthesis one level up."
  (interactive)
  (cond ((looking-at "\\s\(") (forward-list 1))
        (t
         (backward-char 1)
         (cond ((looking-at "\\s\)")
                (forward-char 1) (backward-list 1))
               (t
                (while (not (looking-at "\\s("))
                  (backward-char 1)
                  (cond ((looking-at "\\s\)")
                         (message "->> )")
                         (forward-char 1)
                         (backward-list 1)
                         (backward-char 1)))
                  ))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Includes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'revy)
(require 'revy-ubermode "ubermode.el")
(require 'revy-ubercom "ubercom.el")
(require 'revy-uberinstructions "uberinstructions.el")
(require 'revy-ubersicht "ubersicht.el")
(require 'revy-ubertex "ubertex.el")
(require 'revy-ubermanus "ubermanus.el")
(require 'revy-ubermenu "ubermenu.el")
