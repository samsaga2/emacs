;; package init
(setq package-check-signature nil)
(package-initialize)
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages") t)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; performance improvements
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

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
(load-theme 'tango t)

(setq ring-bell-function 'ignore
      custom-file "custom.el"
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      inhibit-startup-message t
      backup-inhibited t
      auto-save-default nil
      initial-scratch-message ""
      compilation-scroll-output 'first-error
      custom-file "~/.emacs.d/custom.el"
      c-basic-offset 4)

(setq-default indent-tabs-mode nil)

(if (eq system-type 'windows-nt)
    (set-frame-font "Consolas 11" nil t)
  (set-frame-font "Source Code Pro 11" nil t))

(add-to-list 'auto-mode-alist '("\\.qss\\'" . css-mode))


(require 'compile)
(add-hook 'c-mode-common-hook
          (lambda()
            ;; bunch of settings for C-mode 
            (add-to-list 'compilation-error-regexp-alist '("^\\(.*?\\)(\\([0-9]+\\),\\([0-9]+\\)):" 1 2 3))
            ))


;; utf-8
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; packages
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
  :defer t
  :ensure t)

(use-package ivy
  :ensure t
  :config
  ;; (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
        enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume))

(use-package ivy-prescient
  :ensure t
  :config
  (ivy-prescient-mode 1))

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
  :defer t
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
        company-tooltip-limit 1
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t)
  (global-company-mode))

;; (use-package company-box
;;   :ensure t
;;   :hook (company-mode . company-box-mode))

(use-package expand-region
  :ensure t
  :config
  (evil-leader/set-key
    "v" 'er/expand-region))

(defun save-all ()
  (interactive)
  (save-some-buffers t))

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
    "ss" 'save-buffer
    "sa" 'save-all
    "eb" 'eval-buffer
    "fr" 'counsel-recentf
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
    "fd" 'lsp-find-definition
    "fr" 'lsp-find-references
    "<SPC>" 'avy-goto-word-or-subword-1))

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
  (setq lsp-completion-provider :capf)
  :hook ((;; c-mode c++-mode objc-mode
                 prog-mode) .
         (lambda ()
           (require 'ccls)
           (lsp)
           (lsp-headerline-breadcrumb-mode)
           ;(lsp-ui-sideline-mode)
           (lsp-ui-peek-mode)
           (origami-mode)
           )))

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
  ;; Disable client-side cache because the LSP server does a better job.
  (setq company-transformers nil
	company-lsp-async t
	company-lsp-cache-candidates nil
        company-idle-delay 0.0
        company-minimum-prefix-length 1
        ))

(use-package lsp-treemacs
  :defer t
  :ensure t
  :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :defer t
  :ensure t)

(use-package neotree
  :ensure t
  :config
  (setq projectile-switch-project-action 'neotree-projectile-action
        neo-theme 'arrow
        neo-smart-open t
        neo-window-width 50
        neo-window-fixed-size nil
        neo-autorefresh t)
  (evil-leader/set-key
    "pt" 'neotree-projectile-action)
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
  :defer t
  :ensure t)

(use-package smart-mode-line
  :ensure t
  :config
  (setq sml/theme 'light
        sml/no-confirm-load-theme t)
  (sml/setup))

;; (use-package tao-theme
;;   :ensure t
;;   :config
;;   (load-theme 'berrys t))

;; (use-package solarized-theme
;;   :ensure t
;;   :config
;;   (load-theme 'solarized-light t))

(use-package evil-magit
  :ensure t)

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

(use-package clang-format
  :ensure t
  :defer t)

(use-package clang-format+
  :ensure t
  :defer t
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

;; sdz80
(add-to-list 'load-path "~/.emacs.d/vendors/sdz80-mode")
(load "sdz80-mode")
(add-to-list 'auto-mode-alist '("\\.asm" . sdz80-mode))

(load "~/.emacs.d/custom.el")
