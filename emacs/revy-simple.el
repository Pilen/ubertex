(provide 'revy-simple)

(setq inhibit-startup-message t)
(transient-mark-mode t)                   ; show selection from mark
(column-number-mode 1)                    ; show column numbers
(setq-default indent-tabs-mode nil)       ; indent with spaces, not tabs
(setq default-tab-width 4)                ; tab size set to 4 spaces


(cua-mode t)

(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode t)
(setq show-paren-delay 0)

(setq echo-keystrokes 0.01)
(setq linum-eager nil)
(global-linum-mode t)
(setq temporary-file-directory "/tmp/")
(require 'recentf)
(recentf-mode t)
(setq recentf-max-saved-items 10000)
(add-to-list 'recentf-exclude ".ido.last")
(require 'uniquify)
(set-fringe-mode '(0 . 0))

(setq auto-save-default 1)
(setq auto-save-visited-file-name nil) ;;Dont save to current file
(setq delete-auto-save-files 1) ;;Delete autosaves on save
(setq check-auto-save 1) ;;check wether autosave is the most recent on revert-buffer
                                        ;(require 'auto-save)
(setq auto-save-list-file-prefix "~/.emacs.d/auto-save-list/.saves-")
(setq make-backup-files t)
(setq backup-by-copying t)
(setq backup-directory-alist (quote ((".*" . "~/.emacs.d/backups"))))
(setq delete-old-versions t)
(setq default-cursor-type 'box)


(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-create-new-buffer 'always)
(setq ido-file-extension-order '("/" ".tex" ".emacs" ".txt" ".py" ".pl" ".c" ".h" ".hs" ".cfg" ".asm" ".xml" ".org"))
(setq ido-default-buffer-method 'selected-window)
(ido-mode 1)

(global-auto-revert-mode)

(load-theme 'tango-dark t)

(ignore-errors
  (maximize-window))

(setq local-function-key-map (delq '(kp-tab . [9]) local-function-key-map))

(define-minor-mode revy-simple-mode
  "Minor mode for simple keybindings"
  :lighter "simple"
  :global t
  :keymap (let ((revy-simple-mode-map (make-sparse-keymap)))
            (define-key revy-simple-mode-map (kbd "C-s") 'save-buffer)
            (define-key revy-simple-mode-map (kbd "C-f") 'isearch-forward)
            (define-key revy-simple-mode-map (kbd "C-F") 'isearch-backward)
            (define-key revy-simple-mode-map (kbd "C-r") 'replace-string)
            (define-key revy-simple-mode-map (kbd "C-æ") 'other-window)
            (define-key revy-simple-mode-map (kbd "C-b") 'ido-switch-buffer)
            (define-key revy-simple-mode-map (kbd "<menu>") 'ibuffer)
            (define-key revy-simple-mode-map (kbd "C-z") 'undo)

            (define-key revy-simple-mode-map (kbd "C-d") 'kill-line)
            (define-key revy-simple-mode-map (kbd "C-D") 'kill-whole-line)

            (define-key revy-simple-mode-map (kbd "M-i") 'previous-line)
            (define-key revy-simple-mode-map (kbd "M-j") 'left-char)
            (define-key revy-simple-mode-map (kbd "M-k") 'next-line)
            (define-key revy-simple-mode-map (kbd "M-l") 'right-char)
            (define-key revy-simple-mode-map (kbd "M-J") 'beginning-of-line)
            (define-key revy-simple-mode-map (kbd "M-L") 'end-of-line)
            (define-key revy-simple-mode-map (kbd "M-I") 'scroll-down-command)
            (define-key revy-simple-mode-map (kbd "M-K") 'scroll-up-command)

            (define-key revy-simple-mode-map (kbd "<f5>") 'revert-buffer)

            (define-key revy-simple-mode-map (kbd "C-e") 'eshell)
            (define-key revy-simple-mode-map (kbd "M-<tab>") 'next-buffer)
            (define-key revy-simple-mode-map (kbd "M-<tab>") 'previous-buffer)

            ;; (define-key revy-simple-mode-map (kbd "M-<tab>") 'goto-match-paren)
            ;; (define-key revy-simple-mode-map (kbd "M-S-<tab>") 'goto-match-paren)
            (define-key revy-simple-mode-map (kbd "H-<backspace>") 'join-line)

            ;; (define-key revy-simple-mode-map (kbd "<f7>") 'toggle-window-dedicated)



            (define-key revy-simple-mode-map (kbd "<f9>") 'revy-command-center)

            revy-simple-mode-map))

(revy-simple-mode)

(require 'revy)

(progn (find-file (concat
                   (file-name-as-directory
                    (concat
                     (file-name-as-directory revy-ubertex-dir)
                     "emacs"))
                   "help.el"))
       (setq truncate-lines t))
