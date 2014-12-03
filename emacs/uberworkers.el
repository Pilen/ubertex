;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;π Uberinstructions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'revy-uberworkers)

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
