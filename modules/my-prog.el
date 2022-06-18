;; company
(setq company-idle-delay 0.1
      company-show-numbers t
      company-tooltip-limit 10
      company-minimum-prefix-length 1
      company-tooltip-align-annotations t
      company-tooltip-flip-when-above t)

(global-company-mode)

;; smartparens
(setq sp-override-key-bindings
        '(("C-<right>" . nil)
          ("C-<left>" . nil)
          ("C-)" . sp-forward-slurp-sexp)
          ("M-<backspace>" . nil)
          ("C-(" . sp-forward-barf-sexp)))

(smartparens-global-mode)
(sp-use-smartparens-bindings)
(sp--update-override-key-bindings)

;; change the compilation buffer height
(setq compilation-window-height 15)

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

;; add qss as css-mode
(add-to-list 'auto-mode-alist '("\\.qss" . css-mode))

;; c+z80 assembler
(setq-default c-basic-offset 4)
(add-hook 'c-mode-hook 'eglot-ensure)

(add-to-list 'load-path (concat user-emacs-directory "vendors/sdz80-mode"))
(require 'sdz80-mode)

(define-hostmode poly-c-hostmode
  :mode 'c-mode)

(define-hostmode poly-sdz80-hostmode
  :mode 'sdz80-mode)

(define-innermode poly-c-sdz80-innermode
  :mode 'sdz80-mode
  :head-matcher "^[ \t]*__asm"
  :tail-matcher "^[ \t]*__endasm"
  :head-mode 'host
  :tail-mode 'host)

(define-polymode poly-c-mode 
  :hostmode 'poly-c-hostmode
  :innermodes '(poly-c-sdz80-innermode))

(add-to-list 'auto-mode-alist '("\\.c" . poly-c-mode))

;; enable flymake
(require 'flymake)
(add-hook 'prog-mode flymake-mode)

;; enable glsl mode
(require 'glsl-mode)
(add-to-list 'auto-mode-alist '("\\.vert" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag" . glsl-mode))

(provide 'my-prog)
