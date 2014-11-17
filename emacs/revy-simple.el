(provide 'revy-simple)

(fset 'yes-or-no-p 'y-or-n-p)
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)
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

(require 'revy)
