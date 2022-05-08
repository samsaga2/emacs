;;; sdz80-mode.el --- sdz80-mode

;; $Id:$

;; Emacs List Archive Entry
;; Filename: sdz80-mode.el
;; Version: 0.1
;; Keywords:
;; Author: Alexis Roda <alexis.roda.villalonga@gmail.com>
;; Maintainer: Alexis Roda <alexis.roda.villalonga@gmail.com>
;; Created: 2017-09-20
;; Description:
;; URL:
;; Compatibility: Emacs24

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more
;; details. http://www.gnu.org/copyleft/gpl.html

;;; Install:

;; Put this file on your Emacs-Lisp load path and add following into
;; emacs startup file.
;;
;;     (require 'sdz80-mode)
;;
;; or use autoload:
;;
;;      (autoload 'sdz80-mode "sdz80-mode" "" t)

;;; Commentary:
;;


;;; History:
;;

(require 'asm-mode)
(require 'z80opcodes)


;;; Code:

;; (defgroup sdz80-mode nil
;;   "Insert documentation here.")

;; (defcustom sdz80-mode-option nil
;;   "Insert documentation here."
;;   :group 'sdz80-mode
;;   :type  'string
;;   :safe  'stringp)

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Imenu.html

(setq sdz80-imenu
      '((nil "^\s*\\([A-Za-z0-9_$]+\\)\s*:" 1)
        ))

(defconst sdz80-font-lock-keywords
  (append
   '(("\\<\\(a\\|b\\|c\\|d\\|e\\|f\\|h\\|l\\|af\\|bc\\|de\\|hl\\|ix\\|iy\\)\\>"
      1 font-lock-variable-name-face))
   asm-font-lock-keywords)
  "Additional expressions to highlight in sdasm.")


(defun sdz80--locate-directive (directive &optional start)
  "Locate a dot-directive.

Returns the point at the begining of the first line containing
the directive `DIRECTIVE'. If provided start searching at
`START', otherwise start at `point-min'.

If the directive is not found returns nil.

Both point and match data are preserved."
  (let ((res nil)
        (regex (s-concat "^\s*\\." directive "\s*")))
    (save-excursion
      (goto-char (if (null start) (point-min) start))
      (while (and (not (looking-at-p regex))
                  (not (eobp)))
        (forward-line))
      (unless (eobp)
        (setq res (point))))
    res))

(defun sdz80--insert-globl-directive (sap &optional p)
  (goto-char (if (null p) (point) p))
  (tab-to-tab-stop)
  (insert ".globl " sap)
  (newline-and-indent))

(defun sdz80--get-global-symbols ()
  "Return a list with the global labels defined in the current
buffer."
  (save-excursion
    (save-match-data
      (let ((res nil))
        (goto-char (point-min))
        (while (not (eobp))
          (when (looking-at "^\s*\\([[:alpha:]_]+\\)::")
            (setq res (cons (match-string-no-properties 1) res)))
          (forward-line))
        (reverse res)))))

(defun sdz80--get-include-file-name (file-name)
  (let ((res (replace-regexp-in-string "\\.s$" ".h.s" file-name)))
    (if (string-equal file-name res)
        (concat file-name ".h.s")
      res)))

(defun sdz80-colon ()
  "Insert a colon.

Like `asm-colon' but supports sdas's double colon syntax for
declaring global labels."
  (interactive)
  (let ((labelp nil))
    (save-excursion
      (beginning-of-line)
      (when (setq labelp (looking-at-p "^\s*[$.[:alpha:][:digit:]_]+:?\s*$"))
        (delete-horizontal-space)))
    (if (not labelp)
        (insert ":")
     (delete-horizontal-space)
     (insert ":")
     (tab-to-tab-stop))))

(defun sdz80-declare-globl-symbol-at-point ()
  "Insert a '.globl' directive for the symbol at point."
  (interactive)
  (save-excursion
    (save-match-data
      (let ((sym-ap (thing-at-point 'symbol t))
            (p (sdz80--locate-directive "globl")))
        (if (null p)
            ;; No '.globl' directive found. Insert one before the
            ;; first '.area' directive if any, otherwise signal an
            ;; error.
            (let ((q (sdz80--locate-directive "area")))
              (if (null q)
                  (error "No '.globl' or '.area' directive found")
                (sdz80--insert-globl-directive sym-ap q)
                (newline-and-indent)))
          ;; Locate insertion point so that directives are kept
          ;; sorted.
          (let ((ins-point p))
            (while p
              (goto-char p)             ; jump to .globl line
              (right-word)              ; skip .globl directive
              (skip-syntax-forward " ") ; skip whitespace
              (let ((curr-globl (thing-at-point 'symbol t)))
                (cond
                 ((string-equal curr-globl sym-ap)
                  (setq ins-point nil)
                  (setq p nil))
                 ((string-greaterp curr-globl sym-ap)
                  (setq ins-point p)
                  (setq p nil))
                 (t
                  (forward-line)
                  (setq ins-point (point))
                  (setq p (sdz80--locate-directive "globl" (point)))))))
            (when ins-point
              (sdz80--insert-globl-directive sym-ap ins-point))))))))

(defun sdz80-update-dot-h ()
  "Creates/updates 'xxx.h.s' adding '.globl' directives for the
global labels defined in the current buffer."
  (interactive)
  (save-excursion
    (let ((global-symbols (sdz80--get-global-symbols))
          (p (point-min)))
      (find-file (sdz80--get-include-file-name (buffer-file-name)))
      (while p
        (setq p (sdz80--locate-directive "globl" p))
        (when p
          (goto-char p)
          (kill-line)))
      (goto-char (point-min))
      (dolist (i (sort global-symbols 'string-lessp))
        (sdz80--insert-globl-directive i))
      (save-buffer)
      (kill-buffer))))

(setq sdz80-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd ":") 'sdz80-colon)
    (define-key map (kbd "C-c m g") 'sdz80-declare-globl-symbol-at-point)
    (define-key map (kbd "C-c m h") 'sdz80-update-dot-h)
    (define-key map (kbd "C-c m o") 'z80op)
    map))

(define-derived-mode sdz80-mode
  asm-mode
  "sdz80"
  "Major mode for editing z80 assembler for sdas."
  (message "Activant sdz80-mode.")
  (setq imenu-generic-expression sdz80-imenu)
  (setq-local font-lock-defaults '(sdz80-font-lock-keywords nil t))
)


(provide 'sdz80-mode)

;;; sdz80-mode.el ends here
