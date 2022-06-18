;; ctrl-u does page up scroll
(setq evil-want-C-u-scroll t
      evil-want-keybinding nil
      evil-symbol-word-search t)

;; enable evil globally
(evil-mode 1)

;; enable evil surround
(global-evil-surround-mode t)

;; enable evil collection
(evil-collection-init)

;; enable multicursors
(global-evil-mc-mode 1)
(evil-leader/set-key
  "mm" 'evil-mc-make-all-cursors
  "mu" 'evil-mc-undo-all-cursors
  "mb" 'evil-mc-make-cursor-in-visual-selection-beg
  "me" 'evil-mc-make-cursor-in-visual-selection-end)

;; config leader commands
(global-evil-leader-mode t)
(evil-leader/set-leader "<SPC>")
(setq evil-leader/in-all-states t)

(defun save-all ()
  (interactive)
  (save-some-buffers t))

(evil-leader/set-key
  "ff" 'counsel-find-file
  "be" 'val-buffer
  "bb" 'switch-to-buffer
  "bd" 'kill-current-buffer
  "bp" 'switch-to-prev-buffer
  "bn" 'switch-to-next-buffer
  "br" 'counsel-recentf
  "ss" 'save-buffer
  "sa" 'save-all
  "eb" 'eval-buffer
  "gs" 'magit-status
  "wm" 'delete-other-windows
  "wh" 'split-window-vertically
  "wv" 'split-window-horizontally
  "wm" 'delete-other-windows
  "x"  'counsel-M-x
  "hf" 'describe-function
  "hv" 'describe-variable
  "cc" 'comment-or-uncomment-region
  "ee" 'next-error
  "ep" 'previous-error
  "eb" 'eval-buffer
  "hk" 'describe-key
  "hv" 'describe-variable
  "<SPC>" 'avy-goto-word-or-subword-1
  "v" 'er/expand-region
  "pt" 'treemacs)

(evil-leader/set-key-for-mode
  'emacs-lisp-mode
  "bc" 'byte-compile-file)

;; undo tree
(global-undo-tree-mode)
(setq undo-tree-visualizer-timestamps t
      undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))

(evil-leader/set-key
  "u" 'undo-tree-undo
  "au" 'undo-tree-visualize)

(provide 'my-evil)
