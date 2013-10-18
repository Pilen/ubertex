(defstruct revy-screen location display dir) ; user@location:0.screen

(defface revy-cursor-face
  '((((type x w32 mac))
     (:foreground "black" :background "red")))
  "")

(defface revy-local-cursor-face
  '((((type x w32 mac))
     (:foreground "black" :background "4D3B3B")))
  "")

(defface revy-invisible-face
  '((((type x w32 mac))
     (:font "DejaVu Sans Mono")
     (:height 90)
     (:background "gray20")))
  "")

(set-face-attribute 'revy-invisible-face nil :height 50)
(set-face-attribute 'revy-invisible-face nil :font "DejaVu Sans Mono")

(set-face-background 'revy-invisible-face "gray30")
(set-face-foreground 'revy-invisible-face "gray50")
(set-face-background 'revy-cursor-face "firebrick4")

(setq revy-cursor (make-overlay 0 10 (current-buffer) t t))
(overlay-put revy-cursor 'face 'revy-cursor-face)
(overlay-put revy-cursor 'priority 5000)
(overlay-put revy-cursor 'revy t)


(defvar revy-hidden '())
;(make-variable-buffer-local 'revy-hidden)



;(delete-overlay revy-cursor)

(setq line-move-ignore-invisible nil)
(setq revy-currrent-screen (make-revy-screen :location "localhost" :display ":0" :dir "~/2013"))
(setq revy-brok-screen (make-revy-screen :location "revy@192.168.0.100" :display ":0" :dir "~/2013"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar revy-screen-brok (make-revy-screen :location "revy@192.168.0.30" :display ":0" :dir "~/jub"))
(defvar revy-screen-local (make-revy-screen :location "localhost" :display ":0" :dir "~/jub"))
(defvar revy-default-screen revy-screen-brok)
(make-variable-buffer-local 'revy-default-screen)

(defvar revy-stack '())

(defvar revy-cursor) ;; Find out what to do here, perhaps this should be buffer local, but how do you then see which cursor is actually being shown?. Perhaps this should be buffer local in ubersicht but not ubertex? Perhaps we should have both a buffer cursor and a global cursor being on top.
(defvar revy-local-cursor nil)
(make-variable-buffer-local 'revy-local-cursor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;π Ubertex
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun revy-hide ()
  (interactive)
  (revy-unhide)
  (revy-numerize)
  (revy-insert-blank-comments)
  (setq revy-hidden '())
  (save-excursion
    ;; insert missing parrens
    (beginning-of-buffer)
    (while (search-forward-regexp "\\\\elisp{(\\([^(][^)}]+\\)}" nil t)
       (replace-match "\\\\elisp{(\\1)}" nil nil))

    ;; Hide preamble and everything up until the first slide:
    (beginning-of-buffer)
    (search-forward-regexp "\\\\begin{overtex}" nil t)
    (let ((overlay (make-overlay 0 (match-beginning 0) (current-buffer) t nil)))
      ;; (overlay-put overlay 'invisible t)
      (overlay-put overlay 'revy t)
      (overlay-put overlay 'face 'revy-invisible-face)
      (push overlay revy-hidden))

    (beginning-of-buffer)
    (let ((n 1))
      (while (search-forward-regexp "\\\\n{\\([0-9]+\\)}\\|\\\\begin{overtex}\\|\\\\end{overtex}\n\\|\\\\pause\\({}\\)?\\|^\n" nil t)
        (let ((overlay (make-overlay (match-beginning 0) (match-end 0) (current-buffer) t nil)))
          (overlay-put overlay 'revy t)
          (overlay-put overlay 'priority 9000)
          (overlay-put overlay 'face 'revy-invisible-face)
          ;(overlay-put overlay 'invisible t)

          ;; (when (string= (match-string 0) "\\\\begin{overtex})
          (cond ((string= (match-string 0) "\n")
                 ;; (overlay-put overlay 'invisible t)
                 )
                ((string= (match-string 0) "\\end{overtex}\n")
                 ;; (overlay-put overlay 'invisible t)
                 )
                ((or (string= (match-string 0) "\\begin{overtex}")
                     (string= (match-string 0) "\\pause")
                     (string= (match-string 0) "\\pause{}"))
                 (overlay-put overlay 'revy-slide-number n)
                 (incf n)))
          (overlay-put overlay 'revy-slide-number n)
          (when (string= (match-string 0) "\\begin{overtex}")
            ;; (overlay-put overlay 'before-string "")
            ;; (overlay-put overlay 'invisible t)
            )
          (unless (or (string= (match-string 0) "\\pause")
                    (string= (match-string 0) "\\pause{}"))
            (message "kat")
            ;; (let ((icon (make-overlay (- (match-beginning 0) 1) (match-beginning 0) (current-buffer) t nil)))
            ;;       (overlay-put icon 'revy t)
            ;;       (overlay-put icon 'priority 9000)
            ;;       (overlay-put icon 'after-string "$")
            ;;       (push icon revy-hidden))
            (overlay-put overlay 'invisible t)
            )


          ;; (setf revy-hidden (cons overlay revy-hidden))
          (push overlay revy-hidden)
         ;(put-text-property (match-beginning 0) (match-end 0) 'invisible t)
          )))))

(defun revy-numerize ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (let ((n 1))
      (while (search-forward-regexp "\\\\begin{overtex}\\|\\\\pause\\({}\\)?" nil t)
      (insert "\\n{" (int-to-string n) "}")
      (incf n)))))

(defun revy-unnumerize ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (replace-regexp "\\\\n{[0-9]+}" "")))


(defun revy-insert-blank-comments()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (replace-string "\\begin{overtex}\n\\end{overtex}" "\\begin{overtex}\n%blank\n\\end{overtex}")))

(defun revy-unhide ()
  (interactive)
  (revy-unnumerize)
  (remove-text-properties (buffer-end -1) (buffer-end 1) 'bold)
  (remove-overlays (buffer-end -1) (buffer-end 1) 'revy t)
  (remove-overlays (buffer-end -1) (buffer-end 1) 'face 'revy-cursor-face))







;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-forward ()
  (interactive)
  (search-forward-regexp "\\\\begin{overtex}\n\\|\\\\pause\\({}\\)?" nil t))

(defun revy-next ()
  (interactive)
  (goto-char (overlay-end revy-local-cursor))
  (revy-enter))

(defun revy-blank ()
  (interactive)
  (revy-xpdf-goto-slide 1))

(defun revy-enter ()
  (interactive)
  (let ((start nil)
        (end nil))

    ;(save-excursion
      (search-forward-regexp "\\\\end{overtex}\\|\\\\pause\\({}\\)?" nil t)
      (setq end (match-end 0))

    (save-excursion
      (search-backward-regexp "\\\\begin{overtex}" nil t)
      (setq start (match-end 0))
      (when (char-equal (char-after start) ?\n)
        (incf start)))

    (move-overlay revy-local-cursor (+ 7 start) end (current-buffer))
    (move-overlay revy-cursor (+ 7 start) end (current-buffer))
    ;(move-overlay revy-local-cursor (match-end 0) end (current-buffer))
    ;(move-overlay revy-cursor (match-end 0) end (current-buffer))
    (revy-xpdf-goto-slide (revy-slide-number))
    (revy-scan start end)))

;; Doesn't work on overlay properties.
;; (defun revy-slide-number ()
;;   (interactive)
;;   (save-excursion
;;     (search-backward-regexp "\\\\begin{overtex}\\|\\\\pause\\({}\\)?")
;;     (let ((overlays (overlays-at (point)))
;;           (continue t)
;;           (overlay nil))
;;       (while (and continue (not (null overlays)))
;;         (setq overlay (pop overlays))
;;         (when (overlay-get overlay 'revy-slide-number)
;;           (setq continue nil)))
;;       (if (null overlay)
;;           (message "There is no slide")
;;         (print overlay)

;;         (print (overlay-get overlay 'revy-slide-number))
;;         (message (int-to-string (overlay-get overlay 'revy-slide-number)))))))
;; ;    (message (int-to-string (overlay-get (car (overlays-at (point))) 'revy-slide-number)))))


;;Works on revy-numerize
(defun revy-slide-number ()
  (interactive)
  (save-excursion
    (search-backward-regexp "\\\\n{\\([0-9]+\\)}" nil t)
    (message (match-string 1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Ubertex inline commands
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun revy-scan (start end)
;;   (interactive)

;;   (save-excursion
;;     (goto-char end)

;;     (let ((p (save-excursion (progn (search-backward-regexp "\\\\end{overtex}" nil t) (match-end 0)))))
;;       (when (not (= p end))
;;         (search-backward-regexp  "\\\\pause\\({}\\)?" nil t)))

;;     (search-backward-regexp "\\\\begin{overtex}\\|\\\\pause\\({}\\)?" nil t)

;;     (save-excursion
;;       (when (not (null (search-forward-regexp "\\\\shell{\\([^}]+\\)}" end t)))
;;         (message (match-string 1))
;;         (revy-shell (match-string 1))))
;;     (save-excursion
;;       (when (not (null (search-forward-regexp "\\\\elisp{(\\([^)}]+\\)}" end t)))
;;       (revy-elisp (match-beginning 1) (match-end 1))))))

(defun revy-scan (start end)
  (interactive)
  (print end)
  (save-excursion ;should end before evaluating code (so we can jump around)
    (goto-char end)
    (search-backward-regexp "\\\\begin{overtex}\\|\\\\pause[{}]?" nil t) ;; skriv regexes sådan
    (save-excursion
      (when (not (null (search-forward-regexp "\\\\shell{(\\([^}]*\\)}" end t)))
        ;(message (match-string 1))
        (revy-shell (match-string 1))))
    (save-excursion
      (when (not (null (search-forward-regexp "\\\\elisp{\\([^}]*\\)}" end t)))
        ;(message (match-string 1))
        (revy-elisp (match-beginning 1) (match-end 1))))))

  ;; (search-backward-regexp "\\\\pause\\({}\\)?" nil t)
  ;; (goto-char (match-beginning 0)))

  ;; (save-excursion)
  ;;     (search-backward-regexp "\\\\begin{overtex}"))
  ;;     )
      ;; (save-excursion
      ;;   (search-forward-regexp "\\\\shell{\\(.+\\)}" end t)
      ;;   (message (match-string 1))
      ;;   (revy-shell (match-string 1)))
      ;; (search-forward-regexp "\\\\elisp}\\(.+\\)}" end t)
      ;; (revy-elisp (match-string 1)))))

(defun revy-shell (command &optional screen)
  (save-window-excursion
    (if (null screen)
        (call-process-shell-command command nil 0)
      (let ((com
             ;; (concat "ssh "
             ;;                       (revy-screen-location screen)
             ;;                       " -T << EOF \n"
             ;;                       " export DISPLAY="
             ;;                       (revy-screen-display screen)
             ;;                       " ; "
             ;;                       command
             ;;                       " \n EOF ")))
             (concat "ssh " (revy-screen-location screen) " \""
                     "export DISPLAY=" (revy-screen-display screen) ";\n"
                     "cd " (revy-screen-dir screen) ";\n"
                     command
                     ";\"")))
        ;(print com)
        (call-process-shell-command com nil 0)))))
        ;(start-process-shell-command "ubertex" nil com)))))
        ;(async-shell-command com nil nil)))))

(defun revy-elisp (start end)
  (interactive)
  (eval-region start end))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Xpdf
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-ubertex-start ()
  (interactive)
  (revy-hide)
  (revy-shell "killall xpdf" revy-default-screen)
  ;; (revy-shell (concat "cat "(file-name-directory (buffer-file-name)) (file-name-base (buffer-file-name)) ".pdf"
  ;;                     " | ssh " (revy-screen-location revy-current-screen)
  ;;                     " \" mkdir " (revy-screen-dir revy-current-screen) ";"
  ;;                     " cat >> "(revy-screen-dir revy-current-screen) "/" (file-name-base (buffer-file-name)) ".pdf\""))
  ;; (revy-shell (concat (file-name-directory (buffer-file-name)) (file-name-base (buffer-file-name)) ".pdf") revy-current-screen
  (revy-shell (concat "scp " (file-name-directory (buffer-file-name)) (file-name-base (buffer-file-name)) ".pdf"
                      " " (revy-screen-location revy-default-screen) ":~/jub/" (file-name-base (buffer-file-name)) ".pdf"))
  (revy-xpdf-open (concat (revy-screen-dir revy-default-screen) "/" (file-name-base) ".pdf"))
  (beginning-of-buffer)
  (sleep-for 1)
  (revy-enter))

(defun revy-ubertex-restart ()
  (interactive)
  (revy-hide)
  (revy-shell "killall xpdf" revy-default-screen)
  ;; (revy-shell (concat "cat "(file-name-directory (buffer-file-name)) (file-name-base (buffer-file-name)) ".pdf"
  ;;                     " | ssh " (revy-screen-location revy-current-screen)
  ;;                     " \" mkdir " (revy-screen-dir revy-current-screen) ";"
  ;;                     " cat >> "(revy-screen-dir revy-current-screen) "/" (file-name-base (buffer-file-name)) ".pdf\""))
  ;; (revy-shell (concat (file-name-directory (buffer-file-name)) (file-name-base (buffer-file-name)) ".pdf") revy-current-screen
  (revy-shell (concat "scp " (file-name-directory (buffer-file-name)) (file-name-base (buffer-file-name)) ".pdf"
                      " " (revy-screen-location revy-default-screen) ":~/jub/" (file-name-base (buffer-file-name)) ".pdf"))
  (revy-xpdf-open (concat (revy-screen-dir revy-default-screen) "/" (file-name-base) ".pdf"))
  (sleep-for 1)
  (revy-enter)
  )

(defun revy-xpdf-open (file)
  (interactive)
  (revy-shell (concat "xpdf -remote ubertex -fullscreen -mattecolor black -fg black -bg black -papercolor black " file)
              revy-default-screen))

(defun revy-xpdf-goto-slide (slide)
  (let ((slide (if (numberp slide) (int-to-string slide) slide)))
    (revy-shell (concat "xpdf -remote ubertex -exec \\\"gotoPage("
                        slide
                        ")\\\"")
                revy-default-screen)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Keys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;(global-set-key (kbd "<prior>") 'revy-blank)
;; (global-set-key (kbd "<next>") 'revy-next)

;; ;(global-set-key (kbd "<up>") 'revy-backward)
;; ;(global-set-key (kbd "<down>") 'revy-forward)

;; (global-set-key (kbd "<home>") 'revy-ubertex-start)
;; (global-set-key (kbd "<end>") 'revy-enter)

;; (global-set-key (kbd "<delete>") 'revy-blank)
;; (global-set-key (kbd "<f11>") 'revy-hide)
;; (global-set-key (kbd "<f12>") 'revy-unhide)





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;π Ubersicht
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-ubersicht-next ()
  (interactive)
  (goto-char (overlay-end revy-local-cursor))
  (revy-ubersicht-enter))
  ;; (search-forward-regexp "^(")
  ;; (backward-char)
  ;; (let  ((start (point)))
  ;;   (goto-match-paren)
  ;;   (move-overlay revy-local-cursor start (point) (current-buffer))
  ;;   (move-overlay revy-cursor start (point) (current-buffer))
  ;;   (eval-region start (point) standard-output)))

;; Should this behave more like revy-enter?
(defun revy-ubersicht-enter ()
  (interactive)
  ;; (let ((start (search-backward-regexp "^(")))
  ;;   (goto-match-paren)
  ;;   (eval-region start (point) standard-output)))
  (let ((orig (point))
        (start nil)
        (end nil))
    (setq start (search-backward-regexp "^(" nil t))

    (if (null start)
        ;; We are at the start of the buffer  and should evaluate the first slide
        (setq end 0)
      ;; goto end of current slide
      (goto-match-paren)
      (setq end (point)))

    (if (< orig end)
        (progn
          (print "<- eval current")
          (move-overlay revy-local-cursor start end (current-buffer))
          (move-overlay revy-cursor start end (current-buffer))
          (eval-region start end))
      (progn
        (print "-> eval next")
        (search-forward-regexp "^(" nil t) ;; Fails from last slide, continuously marking last slide (except last paren).
        (backward-char)
        (setq start (point))
        (goto-match-paren)
        (move-overlay revy-local-cursor start (point) (current-buffer))
        (move-overlay revy-cursor start (point) (current-buffer))
        (eval-region start (point))
        )

    ))
  ;; (search-forward-regexp "^(")
  ;; (backward-char)
  ;; (goto-match-paren)
  )

(defun revy-open (filename &optional screen)
  (message "running revy-open!")
  (push (current-buffer) revy-stack)
  (find-file-other-window filename)
  (when screen
    (setq revy-default-screen screen))
  (if (string= (downcase (file-name-extension filename)) "tex")
      (revy-ubertex-mode t)
    (revy-ubersicht-mode t)))

(defun revy-nop (&optional &rest _))

(defun revy-quit ()
  (interactive)
  (when revy-ubertex-mode
    (revy-unhide))
  (when revy-ubersicht-mode)
  (pop-to-buffer (pop revy-stack)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;π Overall
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun revy-start ()
  (setq revy-stack '())
  (push (current-buffer) revy-stack)
  (revy-ubersicht-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π Major mode definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-minor-mode revy-ubersicht-mode
  "Minor mode for ubersicht buffers"
  :lighter "ubersicht"
  :keymap (let ((revy-ubersicht-mode-map (make-sparse-keymap)))
            (define-key revy-ubersicht-mode-map (kbd "<next>") 'revy-ubersicht-next)
            (define-key revy-ubersicht-mode-map (kbd "<end>") 'revy-ubersicht-enter)
            revy-ubersicht-mode-map)
  (when (not revy-ubersicht-mode)
    (revy-unhide))
  (when (not (null revy-local-cursor))
    (delete-overlay revy-local-cursor)
    (setq revy-local-cursor nil))
  (when revy-ubersicht-mode
    (setq revy-local-cursor (make-overlay 0 0 (current-buffer) t t))
    (overlay-put revy-local-cursor 'face 'revy-local-cursor-face)
    (overlay-put revy-local-cursor 'priority 4999)
    (overlay-put revy-local-cursor 'revy t)
    (message "kamel")
    (goto-char (point-min)))
  )

(define-minor-mode revy-ubertex-mode
  "Minor mode for ubertex buffers"
  :lighter "ubertex"
  :keymap (let ((revy-ubertex-mode-map (make-sparse-keymap)))
            (define-key revy-ubertex-mode-map (kbd "<next>") 'revy-next)
            (define-key revy-ubertex-mode-map (kbd "<home>") 'revy-ubertex-start)
            (define-key revy-ubertex-mode-map (kbd "<end>") 'revy-enter)
            (define-key revy-ubertex-mode-map (kbd "<delete>") 'revy-blank)
            (define-key revy-ubertex-mode-map (kbd "<f11>") 'revy-hide)
            (define-key revy-ubertex-mode-map (kbd "<f12>") 'revy-unhide)
            revy-ubertex-mode-map)
  (when (not revy-ubertex-mode)
    (revy-unhide))
  (when (not (null revy-local-cursor))
    (delete-overlay revy-local-cursor)
    (setq revy-local-cursor nil))
  (when revy-ubertex-mode
    (setq revy-local-cursor (make-overlay 0 0 (current-buffer) t t))
    (overlay-put revy-local-cursor 'face 'revy-local-cursor-face)
    (overlay-put revy-local-cursor 'priority 4999)
    (overlay-put revy-local-cursor 'revy t)
    (goto-char (point-min)))
  (revy-ubertex-start))


; (defun revy-create-local-cursor ()




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;π Manus
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq revy-overtex-preamble "\\documentclass[14pt]{beamer}
\\usepackage[danish]{babel}
\\usepackage[utf8]{inputenc}
\\usepackage{overtex}

\\begin{document}
\\obeylines

\\begin{overtex}
  % forspil
\\end{overtex}
")


(defun revy-manus-preamble ()
  (interactive)
  (beginning-of-buffer)
  (while (search-forward-regexp "\\\\sings{\\|\\\\scene{" nil t)
    (backward-delete-char 7)
    (insert "%% ")
    (search-forward "}")
    (backward-delete-char 1)
    (when (< (point) (line-end-position))
      (insert "\n"))
    )
  (beginning-of-buffer)
  (search-forward "\\begin{song}")
  (delete-region (point-min) (point))
  (insert revy-overtex-preamble)
  (save-excursion
    ;(search-forward "\\end{song}")
    (replace-string "\\end{song}" "")
    (insert "
\\begin{overtex}
  % slut
\\end{overtex}")
    )
  (indent-region (point-min) (point-max))
)

(defun revy-manus-finish ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (replace-regexp "\\\\pause\n\\\\end{overtex}" "\n\\\\end{overtex}")
    (indent-region (point-min) (point-max))))


;; fanger ikke "text, \pause"
(defun revy-manus-clean ()
  (interactive)
  (beginning-of-buffer)
  (let ((i nil))
    (while (search-forward-regexp "^[[:space:]]*[.,-]" nil t)
      (setq i (read-key-sequence "' '=replace"))
      (when (string= i " ")
        (backward-delete-char 1)
        (move-beginning-of-line nil)))
    (beginning-of-buffer)
    ;; (while (search-forward-regexp "\\([.,-]\\)\\\\pause[[:space:]]*$" nil t)
    ;;   (setq i (read-key-sequence "n=next"))
    ;;   (when (string= i " ")
    ;;     (replace-match "\\pause"))))))
    (while (search-forward-regexp "\\([.,-]\\)\\(\\\\pause\\)?[[:space:]]*$" nil t)
      (goto-char (match-end 1))
      (when (string= " " (read-key-sequence "' '=replace"))
        (backward-delete-char 1)
        (move-beginning-of-line nil))))
  (message "done")
  (revy-manus-finish))


(defun revy-manus-slide ()
  (interactive)
  (when (= (point) (line-beginning-position))
    (backward-char))
  (exchange-point-and-mark)
  (move-beginning-of-line nil)
  (insert "\\begin{overtex}\n")
  (while (< (point) (mark))
    (move-end-of-line nil)
    (when (< (point) (mark))
      (insert "\\pause")
      (move-beginning-of-line nil)
      (next-line)))
  (insert "\n\\end{overtex}")
  (move-beginning-of-line nil)
  (next-line))

(defun revy-manus-pause ()
  (interactive)
  (insert "\\pause\n"))

(defun revy-manus-break ()
  (interactive)
  (insert "\\begin{overtex}\n  % Blank\n\\end{overtex}"))

(defun revy-manus-comment (text)
  (interactive "sComment: ")
  (insert "\\comment{" text "}"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;π testing area
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(revy-open "JegKanIkkeRigtigLadeVaere.tex")


(defun killall (&optional arg)
  (interactive)
  (if (null arg)
    (progn (revy-shell "killall -q feh urxvt xpdf mplayer" revy-screen-brok)
           (revy-shell "killall -q feh urxvt xpdf mplayer" revy-screen-left)
           (revy-shell "killall -q feh urxvt xpdf mplayer" revy-screen-mid)
           (revy-shell "killall -q feh urxvt xpdf mplayer" revy-screen-right))
    (progn
      (revy-shell (concat "killall -q " arg) revy-screen-brok)
      (revy-shell (concat "killall -q " arg) revy-screen-left)
      (revy-shell (concat "killall -q " arg) revy-screen-mid)
      (revy-shell (concat "killall -q " arg) revy-screen-right))))


(defun diend ()
  (interactive)
  (revy-shell "killall -q feh mplayer xpdf" revy-screen-left)
  (revy-shell "killall -q feh mplayer xpdf" revy-screen-mid)
  (revy-shell "killall -q feh mplayer xpdf" revy-screen-right))
(global-set-key (kbd "<print>") 'diend)
