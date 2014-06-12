;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π revy.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; loading this file ensures only two functions are loaded until more is needed.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Installation directory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar revy-ubertex-dir (file-name-directory (directory-file-name (file-name-directory (buffer-file-name))))
  "The revy-ubertex-dir (where ubertex is installed). Is automatically set when revy.el is being loaded.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-create ()
  "Create a new revy interactively.
Will prompt a series of questions and create a revy based on the answers.
Then it will load it"
  (interactive)
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
    (setq ubersicht (concat destination name ".el"))

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

        (beginning-of-buffer)
        (when (yes-or-no-p "Replace all .tex sketches with .el sketches?")
          (replace-regexp "\\(sketches/.*?\\.\\)tex" "\\1el"))

        (beginning-of-buffer)
        (when (yes-or-no-p "Replace all .tex videos with .el videos?")
          (replace-regexp "\\(video/.*?\\.\\)tex" "\\1el"))

        (beginning-of-buffer)
        (replace-regexp "\\(^[^/\n]*/.*?\\.\\(tex\\|el\\)\\)" "(revy-open \"\\1\")")

        (beginning-of-buffer)
        (insert "\n(revy-start)\n\n\n")


        ;; Copy songs:
        (beginning-of-buffer)
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
              (file-name-as-directory (read-string "Default directory on workers: " (concat "~/revy-" (format-time-string "%Y"))))
              "\")\n"
              "\n")

      ;; Workers
      (let ((workers '()))
        (insert "(setq revy-worker-all (make-revy-worker :name\"all\" :port\"\" :location\"\" :display\"\" :dir revy-default-dir))\n")
        (while (yes-or-no-p "Do you want to create a worker?")
          (let ((name "")
                (port "")
                (location "")
                (display "")
                (dir nil)) ;; Should be default dir

            (setq name (read-string "Name: "))
            (while (member (concat "revy-worker-" name) workers)
              (setq name (read-string "Please give a unique name: ")))
            (when (not (yes-or-no-p "Is the worker virtual?"))
              (setq port (read-string "Port: " "9999"))
              (setq location (read-string "Location: " (concat "revy@" name)))
              (setq display (read-string "Display: " ":0"))
              (when (not (yes-or-no-p "Use default directory on worker?"))
                (setq dir (read-string "Dir: "))))
            (insert "(setq revy-worker-" name " (make-revy-worker"
                    " :name\"" name "\""
                    " :port\"" port "\""
                    " :location\"" location "\""
                    " :display\"" display "\""
                    (if (null dir)
                        " :dir revy-default-dir"
                      (concat " :dir\"" dir "\""))
                    "))\n")
            (push (concat "revy-worker-" name) workers)))
        (push "revy-worker-all" workers)

        ;; Default worker
        (insert "\n"
                "(setq-default revy-current-worker "
                (ido-completing-read "Default worker: " (reverse workers) nil t)
                ")\n")))


    ;; Store revy in file containing latest revy.
    (let* ((local (concat (file-name-as-directory revy-ubertex-dir) "local"))
           (latest-revy (concat (file-name-as-directory local) "latest-revy")))
      (when (not (file-exists-p local))
        (make-directory local))
      (with-temp-file latest-revy
        (insert name "\n"
                destination "\n")))

    (message "%s has been created" name)
    (revy-load ubersicht)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Loading
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-load (&optional destination)
  (interactive)

  (let ((local (concat (file-name-as-directory revy-ubertex-dir) "local"))
        (latest-revy (concat (file-name-as-directory local) "latest-revy"))
        (latest-name "")
        (latest-location ""))

    ;; Find latest if wanted and existing
    (when (null destination)
      (when (file-exists-p latest-revy)
        (with-temp-buffer
          (insert-file-contents latest-revy)
          (setq latest-name (progn (search-forward-regexp "^.*?$" nil t) (match-string 0)))
          (setq latest-location (progn (search-forward-regexp "^.*?$" nil t) (match-string 0))))
        (let ((choice (ido-completing-read "Do you want to load: " '(latest-name "Other revy...") nil t)))
          (when (not (string= choice "Other revy..."))
            (when (file-exists-p latest-location)
              (setq destination latest-location))))))

    ;; Find .revy in directory
    (while (or (null destination)
               (not (string= "revy" (file-name-extension destination)))
               (not (file-exists-p destination)))
      (setq destination (expand-file-name (read-directory-name "Please specify revy directory: " destination nil t)))
      (let ((content (directory-files destination t ".*?\\.revy")))
        (when (not (null content))
          (when (= (length content) 1)
            (setq destination (car content)))
          (setq destination (ido-completing-read "Do you want to load: " (nconc content '("No, find other..." nil t)))))))

    ;; Open and load stuff
    (find-file destination)
    (require 'uberrevy)
    (revy-uberrevy-mode 1)
    (revy-ubersicht-mode 1)

    ;; Load settings
    (let ((settings (concat (file-name-sans-extension destination) "-config.el")))
      (if (file-exists-p settings)
          (load-file settings)
        (message "No settings found!")))))

(provide 'revy)
