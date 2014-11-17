(provide 'revy-simple)

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

(define-minor-mode revy-simple-mode
  "Minor mode for simple keybindings"
  :lighter "simple"
  :global t
  :keymap (let ((revy-simple-mode-map (make-sparse-keymap)))
            (define-key revy-simple-mode-map (kbd "C-s") 'save-buffer)
            (define-key revy-simple-mode-map (kbd "C-f") 'isearch-forward)
            (define-key revy-simple-mode-map (kbd "C-F") 'isearch-backward)
            (define-key revy-simple-mode-map (kbd "C-o") 'other-window)
            (define-key revy-simple-mode-map (kbd "C-b") 'ibuffer)
            (define-key revy-simple-mode-map (kbd "C-z") 'undo)

            (define-key revy-simple-mode-map (kbd "C-d") 'kill-line)
            (define-key revy-simple-mode-map (kbd "C-D") 'kill-whole-line)

            (define-key revy-simple-mode-map (kbd "M-i") 'previous-line)
            (define-key revy-simple-mode-map (kbd "M-j") 'left-char)
            (define-key revy-simple-mode-map (kbd "M-k") 'next-line)
            (define-key revy-simple-mode-map (kbd "M-l") 'right-char)
            (define-key revy-simple-mode-map (kbd "M-J") 'beginning-of-line)
            (define-key revy-simple-mode-map (kbd "M-L") 'end-of-line)
            (define-key revy-simple-mode-map (kbd "M-I") 'scroll-up-command)
            (define-key revy-simple-mode-map (kbd "M-K") 'scroll-down-command)

            (define-key revy-simple-mode-map (kbd "<f5>") 'revert-buffer)
            revy-simple-mode-map))

(revy-simple-mode)

(require 'revy)
