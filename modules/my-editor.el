(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance

;; Newline at end of file
(setq require-final-newline t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

;; empty scractch buffer
(setq initial-scratch-message "")

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; load theme
(if (eq 'system-type 'windows-nt)
    (set-frame-font "Cousine 11" nil t)
  (set-face-font 'default "-unknown-DejaVu Sans Mono-normal-normal-normal-*-18-*-*-*-m-0-iso10646-1"))
(load-theme 'doom-acario-light t)
(set-face-attribute 'fringe nil :background nil)

;; enable diff hl
(global-diff-hl-mode t)

;; winum
(setq winum-keymap
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
        (define-key map (kbd "M-1") 'winum-select-window-1)
        (define-key map (kbd "M-2") 'winum-select-window-2)
        (define-key map (kbd "M-3") 'winum-select-window-3)
        (define-key map (kbd "M-4") 'winum-select-window-4)
        (define-key map (kbd "M-5") 'winum-select-window-5)
        (define-key map (kbd "M-6") 'winum-select-window-6)
        (define-key map (kbd "M-7") 'winum-select-window-7)
        (define-key map (kbd "M-8") 'winum-select-window-8)
        (define-key map (kbd "M-9") 'winum-select-window-9)
        map))
(winum-mode)

;; custom file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; enable completion
(require 'selectrum)
(selectrum-mode 1)
(setq selectrum-prescient-mode 1
        prescient-persist-mode 1)

(provide 'my-editor)
