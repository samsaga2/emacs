;; package init
(setq package-check-signature nil)
(package-initialize)
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; global
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(menu-bar-mode -1)
(when (fboundp 'toggle-scroll-bar)
  (toggle-scroll-bar -1)
  (global-hl-line-mode 0))
(tool-bar-mode -1)
(blink-cursor-mode -1)
(column-number-mode t)
(size-indication-mode t)
(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)
(show-paren-mode 1)

(setq ring-bell-function 'ignore
      custom-file "custom.el"
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t
      backup-inhibited t
      auto-save-default nil
      initial-scratch-message ""
      compilation-scroll-output 'first-error
      compilation-ask-about-save nil
      custom-file "~/.emacs.d/custom.el"
      c-basic-offset 4
      tab-width 4
      scroll-margin 4
      large-file-warning-threshold nil
      fill-column 100
      compilation-read-command nil
      warning-minimum-level :emergency)

(setq-default indent-tabs-mode nil)

(if (eq system-type 'windows-nt)
    (set-frame-font "Consolas 11" nil t)
  (set-frame-font "Source Code Pro 11" nil t))


(require 'compile)
(add-hook 'c-mode-common-hook
          (lambda()
            ;; bunch of settings for C-mode 
            (add-to-list 'compilation-error-regexp-alist '("^\\(.*?\\)(\\([0-9]+\\),\\([0-9]+\\)):" 1 2 3))
            ))

;; Keep transient cruft out of ~/.emacs.d/
(setq user-emacs-directory "~/.cache/emacs/"
      backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      url-history-file (expand-file-name "url/history" user-emacs-directory)
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" user-emacs-directory)
      projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-emacs-directory))

;; utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; packages
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one-light t))

(use-package counsel
  :ensure t)

(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t
        evil-want-keybinding nil)
  :config
  (evil-mode)
  (defalias #'forward-evil-word #'forward-evil-symbol)
  (setq-default evil-symbol-word-search t))

(use-package which-key
  :ensure t
  :diminish wich-key-mode
  :config
  (which-key-mode +1))

(use-package magit
  :ensure t)

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))

(use-package selectrum
  :ensure t
  :config
  (selectrum-mode +1)
  (setq selectrum-prescient-mode +1
        prescient-persist-mode +1))

(use-package evil-leader
  :ensure t
  :init
  (global-evil-leader-mode)
  :config
  (evil-leader/set-leader "<SPC>")
  (setq evil-leader/in-all-states t)
  (global-set-key [f8] 'next-error)
  (evil-leader/set-key
    "bb" 'ivy-switch-buffer
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
    "<SPC>" 'avy-goto-word-or-subword-1))

(use-package undo-tree
  :ensure t
  :config
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory))
        undo-tree-enable-undo-in-region nil
        undo-tree-auto-save-history nil)
  (evil-leader/set-key
    "u" 'undo-tree-undo
    "au" 'undo-tree-visualize)
  (global-undo-tree-mode))

(use-package flyspell
  :config
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
	ispell-extra-args '("--sug-mode=ultra")
        flyspell-default-dictionary "english")
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(use-package flycheck
  :ensure t)

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.1
        company-show-numbers t
        company-tooltip-limit 10
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t)
  (global-company-mode))

(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

(use-package expand-region
  :ensure t
  :config
  (evil-leader/set-key
    "v" 'er/expand-region))

(defun save-all ()
  (interactive)
  (save-some-buffers t))

(use-package ivy
  :ensure t)

(use-package ivy-explorer
  :ensure t
  :config
  (ivy-explorer-mode 1))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package linum-relative
  :ensure t
  :config
  (setq linum-relative-backend 'display-line-numbers-mode)
  (linum-relative-global-mode))

(use-package projectile
  :ensure t
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-global-mode)
  (setq projectile-indexing-method 'alien)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (global-set-key [f5] 'projectile-run-project)
  (global-set-key [f9] 'projectile-compile-project)
  (evil-leader/set-key
    "pa" 'projectile-add-known-project
    "pp" 'projectile-switch-project
    "pc" 'projectile-compile-project
    "pr" 'projectile-run-project
    "pR" 'projectile-test-project
    "pf" 'projectile-find-file
    "pi" 'projectile-invalidate-cache
    "pg" 'projectile-grep
    "ft" 'projectile-find-tag
    "pT" 'projectile-regenerate-tags
    "fo" 'projectile-find-other-file))

(use-package lsp-mode
  :commands lsp
  :ensure t
  :config
  ;; (setq lsp-completion-provider :capf)
  :hook ((prog-mode) .
         (lambda ()
           ;; (require 'ccls)
           (lsp)
           (lsp-lens-mode 1)
           (lsp-ui-sideline-mode)
           (lsp-ui-peek-mode)
           (origami-mode)
           (evil-leader/set-key
             "fd" 'lsp-find-definition
             "fr" 'lsp-find-references))))

(use-package lsp-ui
  :ensure t
  :config
  (setq lsp-idle-delay 0.500)
  :commands lsp-ui-mode)

(use-package company-lsp
  :ensure t
  :commands company-lsp
  :config
  (push 'company-lsp company-backends)
  (setq company-transformers '(company-sort-by-occurrence)
        company-idle-delay 0.0
        company-minimum-prefix-length 1))

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :ensure t)

(use-package treemacs
  :ensure t
  :config
  (setq treemacs-git-mode -1
        treemacs-no-png-images t
        treemacs-width 50)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (evil-leader/set-key
    "pt" 'treemacs))

(use-package cmake-mode
  :ensure t)

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/no-confirm-load-theme t
        sml/mode-width 'right
        sml/name-width 60)
  (sml/setup)
  (sml/apply-theme 'respectful))

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :config
  (setq doom-modeline-height 35))

(use-package evil-escape
  :ensure t
  :config
  (setq-default evil-escape-key-sequence "jk")
  (evil-escape-mode))

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t
	auto-package-update-interval 30
        auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package clang-format+
  :ensure t
  :hook ((c-mode c++-mode objc-mode) .
         #'clang-format+-mode))
  
(use-package winum
  :ensure t
  :init
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
  :config
  (winum-mode))

(use-package origami
  :ensure t
  :config
  (global-origami-mode))

(use-package evil-mc
  :ensure t
  :config
  (global-evil-mc-mode 1)
  (evil-leader/set-key
    "mm" 'evil-mc-make-all-cursors
    "mu" 'evil-mc-undo-all-cursors
    "mb" 'evil-mc-make-cursor-in-visual-selection-beg
    "me" 'evil-mc-make-cursor-in-visual-selection-end))

(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode)))

(use-package rjsx-mode
  :ensure t
  :after js2-mode
  :mode (("\\.js\\'" . js2-mode)))

(use-package vue-mode
  :ensure t
  :mode "\\.vue\\'"
  :config
  (add-hook 'vue-mode-hook #'lsp))

(use-package bm
  :ensure t
  :init
  ;; restore on load (even before you require bm)
  (setq bm-restore-repository-on-load t)

  :config
  ;; Allow cross-buffer 'next'
  (setq bm-cycle-all-buffers t)

  ;; save bookmarks
  (setq-default bm-buffer-persistence t)

  ;; Loading the repository from file when on start up.
  (add-hook' after-init-hook 'bm-repository-load)
  ;; Restoring bookmarks when on file find.
  (add-hook 'find-file-hooks 'bm-buffer-restore)

  ;; Saving bookmarks
  (add-hook 'kill-buffer-hook #'bm-buffer-save)

  ;; Saving the repository to file when on exit.  kill-buffer-hook is not called
  ;; when Emacs is killed, so we must save all bookmarks first.
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))

  ;; The `after-save-hook' is not necessary to use to achieve persistence, but
  ;; it makes the bookmark data in repository more in sync with the file state.
  (add-hook 'after-save-hook #'bm-buffer-save)

  ;; Restoring bookmarks
  (add-hook 'find-file-hooks   #'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)

  ;; The `after-revert-hook' is not necessary to use to achieve persistence, but
  ;; it makes the bookmark data in repository more in sync with the file
  ;; state. This hook might cause trouble when using packages that automatically
  ;; reverts the buffer (like vc after a check-in).  This can easily be avoided
  ;; if the package provides a hook that is called before the buffer is reverted
  ;; (like `vc-before-checkin-hook').  Then new bookmarks can be saved before
  ;; the buffer is reverted.  Make sure bookmarks is saved before check-in (and
  ;; revert-buffer)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)

  :bind (("<f2>" . bm-next)
         ("S-<f2>" . bm-previous)
         ("C-<f2>" . bm-toggle)))

;; sdz80
(add-to-list 'load-path "~/.emacs.d/vendors/sdz80-mode")
(load "sdz80-mode")
(add-to-list 'auto-mode-alist '("\\.asm" . sdz80-mode))

;; compilation window height
(setq compilation-window-height 10)

(defun create-proper-compilation-window ()
  "Setup the *compilation* window with custom settings."
  (when (not (get-buffer-window "*compilation*"))
    (save-selected-window
      (save-excursion
        (let* ((w (split-window-vertically))
               (h (window-height w)))
          (select-window w)
          (switch-to-buffer "*compilation*")

          ;; Reduce window height
          (shrink-window (- h compilation-window-height))

          ;; Prevent other buffers from displaying inside
          (set-window-dedicated-p w t))))))

(add-hook 'compilation-mode-hook 'create-proper-compilation-window)

(load "~/.emacs.d/custom.el")
