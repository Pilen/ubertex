;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;π Uberlib
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helperfunctions
(provide 'revy-uberlib)


;; Todo lets use string-prefix-p and string-suffix-p instead
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

(defun revy-relative-data-path (file &optional alternative-extension)
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

(defun revy-absolute-data-path (file &optional alternative-extension)
  "Returns the relative filepath as an absolute path from revy-dir.
 If the file is already absolute, nothing is done."

  ;; Replace file extension.
  (when alternative-extension
    (setq file (revy-replace-extension file alternative-extension)))

  (if (file-name-absolute-p file)
      file
    (revy-join-path revy-dir file)))


(defun revy-define-buffer-key (key command)
  "Define a keyboard shortcut for the majormode locally in the current buffer"
  (use-local-map (copy-keymap (current-local-map)))
  (local-set-key key command))

(defun revy-on-worker (worker function &rest args)
  "Run instruction on another worker than the current
Used like (revy-on-worker revy-worker-brok 'revy-play-sound \"sound.mp3\")"
  (let ((revy-current-worker worker))
    (apply function args)))

(defun revy-join-path (dir &rest dirs)
  "Join dirs into a single path.
Does not expand it nor makes it absolute, use `expand-file-name' for that"
  (let ((path dir))
    (dolist (dir dirs)
      (setq path (concat (file-name-as-directory path) dir)))
    path))

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
