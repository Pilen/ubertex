;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;π Uberprepare
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'revy-uberprepare)

(defvar revy--compile-processes nil
  "List of processes compiling latex files.
 We shouldn't sync till these are finished.")

(defun revy-build ()
  "Build all .tex files"
  (interactive)
  (save-some-buffers)
  (let ((directories (list revy-dir))
        (dir nil))

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
        (sleep-for 0 200)))
    (setq revy--compile-processes nil)
    (accept-process-output)
))
    ;; (revy-sync-files)))



(defun revy-compile-tex (&optional file)
  "Compile a .tex file if it has been changed since last compile.
If no filename is given, the current buffer
Returns the process used to compile the tex file"
  (interactive)
  (when (null file)
    (setq file (buffer-file-name)))

  (unless (file-exists-p file)
    (error "File does not exist: %s" file))

  (lexical-let* ((short (revy-data-path file))
                 (tex file)
                 (pdf (revy-replace-extension file "pdf"))
                 (process nil)
                 (buffer nil))

    (when (file-newer-than-file-p tex pdf)
      (setq buffer (generate-new-buffer (concat "*revy-compile-" short "*")))
      (setq process (start-process (concat "revy-compile-" short)
                                   buffer
                                   "pdflatex"
                                   "-halt-on-error"
                                   "-file-line-error"
                                   ;; "-interaction" "nonstopmode"
                                   "-output-directory" (file-name-directory tex)
                                   tex))
      (set-process-sentinel process
                            (lambda (p event)
                              (unless (process-live-p process)
                                (if (= (process-exit-status process) 0)
                                    (message "%s compiled successfully" short)
                                  (let ((error-message nil)
                                        (error-line nil))
                                    (with-current-buffer buffer
                                      (goto-char (point-max))
                                      (search-backward-regexp "^.*==> Fatal error")
                                      (search-backward-regexp
                                       (concat tex (rx ":" (group (+ digit)) ":" (* whitespace) (group (+ (not (any "."))) "."))))
                                      (setq error-line (string-to-int (match-string 1))
                                            error-message (replace-regexp-in-string "\n" "" (match-string 2))))
                                    (find-file tex)
                                    ;; (goto-line error-line)
                                    (goto-char (point-min))
                                    (forward-line (1- error-line))
                                    (message "An error ocurred during compilation of %s: %s" short error-message)))

                                ;; (message "elefant")
                                ;; (print buffer)
                                (when buffer
                                  (kill-buffer buffer))

                                ;; Delete auxiliary files
                                (dolist (ext '("aux" "log" "nav" "out" "snm" "toc"))
                                  (let ((file (revy-replace-extension tex ext)))
                                    (when (file-exists-p file)
                                      (delete-file file)))))))
      process)))
