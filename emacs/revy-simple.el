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

(menu-bar-mode -1)
(tool-bar-mode -1)
(define-minor-mode revy-simple-mode
  "Minor mode for simple keybindings"
  :lighter "simple"
  :global t
  :keymap (let ((revy-simple-mode-map (make-sparse-keymap)))
            (define-key revy-simple-mode-map (kbd "<f9>") 'revy-ubermenu)
            (define-key revy-simple-mode-map (kbd "C-o") 'ido-find-file)

            (define-key revy-simple-mode-map (kbd "C-s") 'save-buffer)
            (define-key revy-simple-mode-map (kbd "C-d") 'save-buffer)

            (define-key revy-simple-mode-map (kbd "C-e") 'end-of-line)
            (define-key revy-simple-mode-map (kbd "C-k") 'kill-line)
            (define-key revy-simple-mode-map (kbd "C-S-k") 'kill-whole-line)
            (define-key revy-simple-mode-map (kbd "H-<backspace>") 'join-line)

            (define-key revy-simple-mode-map (kbd "C-f") 'isearch-forward)
            (define-key revy-simple-mode-map (kbd "C-S-f") 'isearch-backward)
            (define-key revy-simple-mode-map (kbd "C-r") 'query-replace)

            (define-key revy-simple-mode-map (kbd "C-z") 'undo)

            (define-key revy-simple-mode-map (kbd "C-t") 'eshell)

            (define-key revy-simple-mode-map (kbd "C-b") 'ido-switch-buffer)
            (define-key revy-simple-mode-map (kbd "<menu>") 'ibuffer)

            (define-key revy-simple-mode-map (kbd "<f5>") 'delete-window)
            (define-key revy-simple-mode-map (kbd "<f6>") 'split-window-horizontally)
            (define-key revy-simple-mode-map (kbd "<f7>") 'revy-simple-setup-windows)
            (define-key revy-simple-mode-map (kbd "<f8>") 'other-window)

            (define-key revy-simple-mode-map (kbd "C-æ") 'other-window)
            ;; (define-key revy-simple-mode-map (kbd "C-p") 'split-window-horizontally)
            ;; (define-key revy-simple-mode-map (kbd "M-p") 'split-window-vertically)

            ;; (define-key revy-simple-mode-map (kbd "C-'") 'delete-window)
            ;; (define-key revy-simple-mode-map (kbd "M-*") 'delete-other-windows)
            (define-key revy-simple-mode-map (kbd "C-ø") 'balance-windows)

            (define-key revy-simple-mode-map (kbd "<f1>") 'revy-manus-insert-comment)


            ;; (define-key revy-simple-mode-map (kbd "M-<tab>") 'goto-match-paren)
            ;; (define-key revy-simple-mode-map (kbd "M-S-<tab>") 'goto-match-paren)

            ;; (define-key revy-simple-mode-map (kbd "<f7>") 'toggle-window-dedicated)


            revy-simple-mode-map))

(revy-simple-mode)

(define-key isearch-mode-map (kbd "C-f") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "C-S-F") 'isearch-repeat-backward)


(defun revy-simple-setup-windows ()
  "Setup windows in a sane order"
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (previous-buffer))

(require 'revy)
(require 'revy-ubermenu "ubermenu.el")


(progn (find-file (concat
                   (file-name-as-directory
                    (concat
                     (file-name-as-directory revy-ubertex-dir)
                     "emacs"))
                   "help.el"))
       (setq truncate-lines t))
