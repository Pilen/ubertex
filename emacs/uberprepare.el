;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;π Uberprepare
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'revy-uberprepare)

(defvar revy--compile-processes nil
  "List of processes compiling latex files.
 We shouldn't sync till these are finished.")

;; Todo only upload when all compiles succeeded
;; Todo deactivate/reactivate all ubertex modes/ so they are rerun, aka so we renumber slides.
;;      Is it compile that should do this? but only if the compiled file is the current-buffer (think about building during the show)
(defun revy-build ()
  "Build all .tex files"
  (interactive)
  (save-some-buffers)
  (let ((directories (list revy-dir))
        (dir nil)
        (failed nil))

    (while directories
      (setq dir (car directories))
      (setq directories (cdr directories))
      (dolist (file (directory-files dir t "[^.]\\{1,2\\}"))
        (if (file-directory-p file)
            (push file directories)
          (when (and (file-name-extension file)
                     (string= "tex" (file-name-extension file)))
            (push (revy-compile-tex file) revy--compile-processes)))))

    (dolist (process revy--compile-processes)
      ;; Busy wait for process to finish
      (while (process-live-p process)
        (sleep-for 0 200))
      (unless (= (process-exit-status process) 0)
        (setq failed t)))
    (setq revy--compile-processes nil)
    (accept-process-output)
    (if failed
        (message "Compilation failed, nothing uploaded")
      (revy-upload-files))))



(defun revy-compile-tex (&optional file)
  "Compile a .tex file if it has been changed since last compile.
If no filename is given, the current buffer is compiled.
Returns the process used to compile the tex file"
  (interactive)
  (when (null file)
    (setq file (buffer-file-name)))

  (unless (file-exists-p file)
    (error "File does not exist: %s" file))

  (lexical-let* ((short (revy-relative-data-path file))
                 (tex file)
                 (pdf (revy-replace-extension file "pdf"))
                 (process nil)
                 (buffer nil))

    (when (file-newer-than-file-p tex pdf)
      (setq buffer (generate-new-buffer (concat "*revy-compile-" short "*")))
      (let* ((sty (concat "TEXINPUTS=" revy-ubertex-dir ":" "$TEXINPUTS" ":" ".//" ":"))
             (process-environment (cons sty process-environment)))
        (setq process (start-process (concat "revy-compile-" short)
                                     buffer
                                     "pdflatex"
                                     "-halt-on-error"
                                     "-file-line-error"
                                     "-interaction" "nonstopmode"
                                     "-output-directory" (file-name-directory tex)
                                     tex)))
      (set-process-sentinel process
                            (lambda (p event)
                              (unless (process-live-p process)
                                (if (= (process-exit-status process) 0)
                                    (message "%s compiled successfully" short)
                                  (let ((error-message nil)
                                        (error-line nil))
                                    (with-current-buffer buffer
                                      (goto-char (point-max))
                                      (when (search-backward-regexp "^.*==> Fatal error" nil t)
                                        (search-backward-regexp
                                         (concat tex (rx ":" (group (+ digit)) ":" (* whitespace) (group (+ (not (any "."))) "."))))
                                        (setq error-line (string-to-int (match-string 1))
                                              error-message (replace-regexp-in-string "\n" "" (match-string 2))))
                                      (when (search-backward-regexp "! LaTeX Error: \\(.*\\)\\.\n" nil t)
                                        (setq error-line 0)
                                        (setq error-message (match-string 1))))

                                    (find-file tex)
                                    ;; (goto-line error-line)
                                    (goto-char (point-min))
                                    (forward-line (1- error-line))
                                    (message "An error ocurred during compilation of %s: %s" short error-message)))

                                ;; (print buffer)
                                (when buffer
                                  (kill-buffer buffer))

                                ;; Delete auxiliary files
                                (dolist (ext '("aux" "log" "nav" "out" "snm" "toc"))
                                  (let ((file (revy-replace-extension tex ext)))
                                    (when (file-exists-p file)
                                      (delete-file file)))))))
      process)))
