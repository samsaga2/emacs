;; global
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(blink-cursor-mode -1)
(setq ring-bell-function 'ignore
      custom-file "custom.el"
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t
      backup-inhibited t
      auto-save-default nil
      initial-scratch-message "")

(column-number-mode t)
(size-indication-mode t)

(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)
(setq-default indent-tabs-mode nil)

(if (eq system-type 'windows-nt)
    (set-frame-font "Consolas 12" nil t)
  (set-frame-font "Monospace 12" nil t))

;; utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; package init
(package-initialize)
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

;; packages
(use-package evil
  :ensure t
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode))

(use-package which-key
  :ensure t
  :diminish wich-key-mode
  :config
  (which-key-mode +1))

(use-package magit
  :ensure t)

(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume))

(use-package swiper
  :ensure t
  :config
  (global-set-key "\C-s" 'swiper))

(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char))

(use-package undo-tree
  :ensure t
  :config
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
	`((".*" . ,temporary-file-directory)))
  (setq undo-tree-auto-save-history t)
  (evil-leader/set-key
    "au" 'undo-tree-visualize))

(use-package flyspell
  :config
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
	ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(use-package flycheck
  :ensure t)

(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0.1)
  (setq company-show-numbers t)
  (setq company-tooltip-limit 10)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  (setq company-tooltip-flip-when-above t)
  (global-company-mode))

(use-package expand-region
  :ensure t)

(use-package evil-leader
  :ensure t
  :init
  (global-evil-leader-mode)
  :config
  (evil-leader/set-leader "<SPC>")
  (setq evil-leader/in-all-states t)
  (evil-leader/set-key
    "bb" 'ivy-switch-buffer
    "bd" 'kill-current-buffer
    "bp" 'switch-to-prev-buffer
    "bn" 'switch-to-next-buffer
    "fs" 'save-buffer
    "fr" 'counsel-recentf
    "gs" 'magit-status
    "wm" 'delete-other-windows
    "wv" 'split-window-vertically
    "wh" 'split-window-horizontally
    "wm" 'delete-other-windows
    "x"  'counsel-M-x))

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
  (setq projectile-indexing-method 'alien)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (projectile-mode +1)
  (evil-leader/set-key
    "pa" 'projectile-add-known-project
    "pp" 'projectile-switch-project
    "pc" 'projectile-compile-project
    "pf" 'projectile-find-file
    "pi" 'projectile-invalidate-cache
    "fo" 'projectile-find-other-file))

(use-package ccls
  :ensure t)

(use-package lsp-mode
  :ensure t
  :init (setq lsp-keymap-prefix "s-l")
  :commands lsp
  :config
  (setq lsp-disabled-clients '(clangd)
	lsp-enable-file-watchers nil
	lsp-enable-snippet nil)
  (add-hook 'prog-mode-hook #'lsp))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package company-lsp
  :ensure t
  :commands company-lsp
  :config
  (push 'company-lsp company-backends)
  ;; Disable client-side cache because the LSP server does a better job.
  (setq company-transformers nil
	company-lsp-async t
	company-lsp-cache-candidates nil))

(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :ensure t)

(use-package neotree
  :ensure t
  :config
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (evil-leader/set-key
    "pt" 'neotree-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
  (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
  (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
  (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle))

(use-package cmake-mode
  :ensure t)

(use-package telephone-line
  :ensure t
  :config
  (setq telephone-line-primary-left-separator 'telephone-line-gradient
	telephone-line-secondary-left-separator 'telephone-line-nil
	telephone-line-primary-right-separator 'telephone-line-gradient
	telephone-line-secondary-right-separator 'telephone-line-nil)
  (setq telephone-line-height 24
	telephone-line-evil-use-short-tag t)
  (telephone-line-mode 1))

(use-package minimal-theme
  :ensure t
  :config
  (load-theme 'minimal-light t))

(use-package evil-magit
  :ensure t)

(use-package evil-escape
  :ensure t
  :config
  (setq-default evil-escape-key-sequence "jk")
  (evil-escape-mode))

(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t
	auto-package-update-interval 4)
  (auto-package-update-maybe))

;; sdz80
(add-to-list 'load-path "~/.emacs.d/vendors/sdz80-mode")
(load "sdz80-mode")
(add-to-list 'auto-mode-alist '("\\.asm" . sdz80-mode))

(load "~/.emacs.d/custom.el")
