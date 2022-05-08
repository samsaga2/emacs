;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1)
  (toggle-scroll-bar -1))

(menu-bar-mode -1)

;; maximize
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; enable hl line everywere
(global-hl-line-mode t)

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; show line numbers at the beginning of each line
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; modeline
(setq sml/no-confirm-load-theme t
      sml/mode-width 'right
      sml/name-width 60
      sml/shorten-modes t
      sml/shorten-directory t)
(sml/setup)
(sml/apply-theme 'light)
(set-face-attribute 'mode-line nil
                    :background "#dddddd"
                    :foreground "black"
                    :box '(:line-width 8 :color "#dddddd")
                    :overline nil
                    :underline nil)
(set-face-attribute 'mode-line-inactive nil
                    :background "#eeeeee"
                    :foreground "black"
                    :box '(:line-width 8 :color "#eeeeee")
                    :overline nil
                    :underline nil)

(provide 'my-ui)
