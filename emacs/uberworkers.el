;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;π Uberworkers
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'revy-uberworkers)

;; internal format of workers:
;; [revy-worker location port user display dir installation channel]
;; Where revy-worker is a constant atom and the others are variables
(defconst revy-worker-user-index 1)
(defconst revy-worker-location-index 2)
(defconst revy-worker-port-index 3)
(defconst revy-worker-display-index 4)
(defconst revy-worker-dir-index 5)
(defconst revy-worker-installation-index 6)
(defconst revy-worker-channel-index 7)


(defun revy-create-workers (&rest workers)
  "Create the workers.
All previous workers are deleted and only the ones specified will exist.
Each worker is a list
"
  (setq revy-workers (make-hash-table))
  (dolist (worker workers)
    (apply 'revy--create-worker worker)))

(defun revy--create-worker (names user location &optional port display dir installation)
  "Create a worker
`names' is a list of symbols, the names of the worker.
`user' is the user on the location.
`location' is the location on the network including username and IP or hostname eg. revy@192.168.0.123 or revy@brok.
`port' is the listening port of zeigen on the worker. nil means the default is used.
`display' is the display the worker should display at, most likely :0 if it has only one display. nil means the default is used.
`dir' is the directory on the worker where all files for the revy are stored. nil means the default is used.

'all is automatically added to the list of names.
The workers are stored in an internal datastructure, for use in elisp simply use one of the names as symbols"
  (when (integerp port)
    (setq port (int-to-string port)))

  (when dir
    (setq dir (file-name-as-directory dir)))

  (setq port (or port revy-worker-default-port))
  (setq display (or display revy-worker-default-display))
  (setq dir (or dir revy-worker-default-dir))
  (setq installation (or installation revy-worker-default-installation))

  (let ((worker (vector 'revy-worker user location port display dir installation nil)))
    (dolist (name names)
      (let ((worker-list (gethash name revy-workers)))
        (if worker-list
            (nconc worker-list (list worker))
          (puthash name (list worker) revy-workers))))))

(defun revy-get-workers (name)
  "Return the worker with name, or nil if no such worker exists.
Does not ensure the worker is actually live."
  (gethash name revy-workers))
