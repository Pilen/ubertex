;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π revy.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; loading this file ensures only two functions are loaded until more is needed.
(provide 'revy)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Installation directory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst revy-ubertex-dir
  (file-name-directory
   (directory-file-name
    (file-name-directory
     (or load-file-name
         buffer-file-name))))
  "The revy-ubertex-dir
This is where ubertex is installed.
Is automatically set when revy.el is being loaded or evaluated")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-create ()
  "Create a new revy interactively.
Will prompt a series of questions and create a revy based on the answers.
Then it will load it"
  (interactive)

  ;; Load libraries, (the user decided to use the revy, so it is safe to load (and polute) the environment)
  (require 'revy-uberrevy "uberrevy.el")

  (let ((name (read-string "Name: "))
        (destination "")
        (plan "")
        (path default-directory)
        (ubersicht "")
        (songs ""))

    ;; Ensure a valid name is supplied
    (while (string= "" name)
      (setq name (read-string "Name (for the revy): ")))
    (setq name (replace-regexp-in-string "[[:blank:]/]" "-" name))

    ;; Ask to append year
    (let* ((year (format-time-string "%Y"))
           (name-year (concat name "-" year)))
      (when (not (revy-string-ends-with name year))
        (when (yes-or-no-p (concat "do you want to append the year: " name-year))
          (setq name name-year))))

    ;; Find destination (directory)
    (setq destination (expand-file-name (read-directory-name "Destination: " nil nil nil)))
    (when (not (file-exists-p destination))
      ;; Should this be done right before creating the files instead?
      (make-directory destination))

    ;; Should a subdir be created (from name)
    (when (yes-or-no-p (concat "Should the revy be placed in subfolder \"" name "/\"?"))
      (setq destination (concat (file-name-as-directory destination) name))
      ;; Will abort function if this subdir already exists
      (make-directory destination))


    (setq destination (file-name-as-directory destination))
    (setq ubersicht (concat destination name ".revy"))

    ;; Create ubersicht
    ;; Does source exist?
    (if (not (yes-or-no-p "Does a tex source of materials exist? (else the revy will be created from scratch)"))
        ;; No, create an empty ubersicht.
        (with-temp-file ubersicht
          (insert "Akt 1\n\n")
          (insert "Akt 2\n\n")
          (insert "Akt 3\n\n")
          (insert "Ekstranumre\n\n"))

      ;; Yes, create from existing
      ;; Find the source
      ;; (read-file-name "Find the .plan file: " nil nil t nil
      ;;                 (lambda (filename)
      ;;                   (or (string= filename
      ;;                                (file-name-as-directory filename))
      ;;                       (string= (file-name-extension filename) "plan"))))
      (setq plan (expand-file-name (read-file-name "Find the .plan file: " nil nil t)))
      (while (not (string= "plan" (file-name-extension plan)))
        (setq path (expand-file-name (read-file-name "Please specify a \".plan\" file: " (file-name-directory path) nil t)))
        (setq plan path)
        (when (not (string= "plan" (file-name-extension plan)))
          (when (not (file-directory-p plan))
            (setq plan (file-name-directory plan)))
          (setq plan (directory-files plan t ".*?\\.plan"))
          (if (null plan)
              (setq plan "")
            (setq plan (ido-completing-read "Did you mean: " (nconc plan '("no")))))))

      (with-temp-file ubersicht
        (insert-file-contents plan)

        ;; (search-forward-regexp "^[^/]*/[^.]*.tex")
        ;; (search-forward-regexp "sketches/[^.]*.tex")
        ;; (search-forward-regexp "\\(sketches/[^.]*\\).tex")

        (goto-char (point-min))
        (when (yes-or-no-p "Create .sketch file for each sketch?")
          (replace-regexp "\\(sketches/.*?\\.\\)tex" "\\1sketch")
          (goto-char (point-min))
          (let ((dir (file-name-as-directory (concat destination "sketches"))))
            (make-directory dir t)
            (while (search-forward-regexp "sketches/\\(.*?\\.sketch\\)" nil t)
              (print dir)
              (print (match-string 1))
              (write-region "\n\n\n(revy-end-sketch)" nil (concat dir (match-string 1)))
              )))

        (goto-char (point-min))
        (when (yes-or-no-p "Create .sketch file for each video?")
          (replace-regexp "\\(video/.*?\\.\\)tex" "\\1sketch")
          (goto-char (point-min))
          (let ((dir (file-name-as-directory (concat destination "sketches"))))
            (make-directory dir t)
            (while (search-forward-regexp "video/\\(.*?\\.sketch\\)" nil t)
              (print dir)
              (print (match-string 1))
              (write-region "\n\n\n(revy-end-sketch)" nil (concat dir (match-string 1)))
              )))

        (goto-char (point-min))
        (replace-regexp "\\(^[^/\n]*/.*?\\.\\(tex\\|sketch\\)\\)" "(revy-open \"\\1\")")

        (goto-char (point-min))
        (insert "\n(revy-start)\n\n\n")


        ;; Copy songs:
        (goto-char (point-min))
        ;; Only if wanted
        (when (yes-or-no-p "Do you want to copy over songtexts?")
          (let ((songs (file-name-as-directory (concat (file-name-directory plan) "sange")))
                (to (file-name-as-directory (concat destination "sange"))))
            ;; If default destination is not found ask for one
            (when (not (file-exists-p songs))
              (setq songs (read-directory-name "Please specify the song directory: " nil nil t)))
            ;; Only proceed if song dir actually exists
            (if (not (file-exists-p songs))
                (message "Could not find the directory, no songs copied.")

              (when (not (file-exists-p to))
                (make-directory to))
              (while (search-forward-regexp "\"sange/\\(.*?.tex\\)\"" nil t)
                (let ((file (concat songs (match-string 1))))
                  (message "Copying file: %s" file)
                  (copy-file file to 1)))))))) ;; Ask when file already exists (the number)

    (with-temp-file (concat destination name "-config.el")

      ;; Default directories
      (insert "(setq revy-dir \"" destination "\")\n"
              "(setq revy-worker-default-dir \""
              (file-name-as-directory (read-string "Default directory on workers: " (concat "~/" name)))
              "\")\n"
              "(setq revy-worker-default-installation \""
              (file-name-as-directory (read-string "Default installation directory on workers: " revy-worker-default-installation))
              "\")\n"
              "\n")

      ;; Workers
      (let ((workers nil))
        (insert "(revy-create-workers\n")
        (while (yes-or-no-p "Do you want to create a worker?")
          (let ((name "")
                first-name
                names
                port
                location-pair
                location
                user
                display
                dir
                installation)

            ;; Names
            ;; TODO: Ensure all names are valid symbols
            (while (string= name "")
              (setq name (read-string "Name: ")))
            (setq first-name name)
            (setq names (list name))
            (push name workers)
            (while (not (string= name ""))
              (setq name (read-string "Additional name? (leave blank to continue): " nil 'workers))
              (unless (string= name "")
                (add-to-list 'names name t)
                (push name workers)))
            (setq location-pair (split-string
                                 (read-string "Location: " (concat "revy@" first-name))
                                 "@"))
            (if (= (length location-pair) 2)
                (progn (setq user (car location-pair))
                       (setq location (cadr location-pair)))
              (setq location (car location-pair))
              (setq user (read-string "User at location: ")))
            (setq port (read-string "Port: " revy-worker-default-port))
            (setq display (read-string "Display: " revy-worker-default-display))
            (when (not (yes-or-no-p "Use default directory on worker?"))
              (setq dir (read-string "Dir: ")))
            (when (not (yes-or-no-p "Use default installation directory on worker?"))
              (setq installation (read-string "Installation: ")))

            (insert " (list "
                    "'(" (mapconcat 'identity names " ") ") "
                    "\"" user "\" "
                    "\"" location "\" "
                    (if port (concat "\"" port "\"") "nil") " "
                    (if display (concat "\"" display "\"") "nil") " "
                    (if dir (concat "\"" dir "\"") "nil") " "
                    (if installation (concat "\"" installation"\"") "nil"))
            (insert ")\n")))
        (delete-backward-char 1)
        (insert ")\n\n")

        ;; Default worker
        (insert "(setq-default revy-current-worker '"
                (ido-completing-read "Default worker: " (reverse workers) nil t)
                ")\n")))

    ;; Store revy in file containing latest revy.
    (let* ((local (concat (file-name-as-directory revy-ubertex-dir) "local"))
           (latest-revy (concat (file-name-as-directory local) "latest-revy")))
      (when (not (file-exists-p local))
        (make-directory local))
      (with-temp-file latest-revy
        (insert name "\n"
                ubersicht "\n")))

    (message "%s has been created" name)
    (revy-load ubersicht)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Loading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-load (&optional destination)
  "Load a revy, and execute its settings.

If current buffer is a revy, load it.
Else ask if the latest created revy should be used.
Else try to find it somewhere."
  (interactive)

  (let* ((local (concat (file-name-as-directory revy-ubertex-dir) "local"))
         (latest-revy (concat (file-name-as-directory local) "latest-revy"))
         (latest-name "")
         (latest-location ""))

    (when (and (null destination)
               (buffer-file-name)
               (string= (file-name-extension buffer-file-name)
                        "revy"))
      (setq destination buffer-file-name))


    ;; Find latest if wanted and existing
    (when (null destination)
      (when (file-exists-p latest-revy)
        (with-temp-buffer
          (insert-file-contents latest-revy)
          (setq latest-name (progn (search-forward-regexp "^.*?$" nil t) (match-string 0)))
          (setq latest-location (progn (search-forward-regexp "^.*?$" nil t) (match-string 0))))
          (print latest-name)
        (let ((choice (ido-completing-read "Do you want to load: " (list latest-name "Other revy...") nil t)))
          (when (not (string= choice "Other revy..."))
            (when (file-exists-p latest-location)
              (setq destination latest-location))))))

    ;; Find .revy in directory
    (while (or (null destination)
               (not (string= "revy" (file-name-extension destination)))
               (not (file-exists-p destination)))
      (setq destination (expand-file-name (read-directory-name "Please specify revy directory: " destination nil t)))
      (let ((content (directory-files destination t ".*?\\.revy$")))
        (when (not (null content))
          (if (= (length content) 1)
              (setq destination (car content))
            (setq destination (ido-completing-read "Do you want to load: " (nconc content '("No, find other..." nil t))))))))

    ;; Open and load stuff
    (require 'revy-uberrevy "uberrevy.el")
    (find-file destination)
    (setq revy-file destination)
    (setq revy-buffer (current-buffer))
    (revy-ubersicht-mode)

    (global-revy-mode)

    ;; Load settings
    (let ((settings (concat (file-name-sans-extension destination) "-config.el")))
      (if (file-exists-p settings)
          (load-file settings)
        (message "No settings found!")))))
