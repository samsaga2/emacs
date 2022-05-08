(require 'package)
(require 'cl-lib)

;; accessing a package repo over https on Windows is a no go, so we
;; fallback to http there
(if (eq system-type 'windows-nt)
    (add-to-list 'package-archives
                 '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/") t))

(package-initialize)
(defvar my-packages
  '(bm
    cider
    cider-eval-sexp-fu
    clj-refactor
    clojure-mode
    cmake-mode
    company
    company-box
    counsel
    diff-hl
    doom-themes
    eglot
    evil
    evil-collection
    evil-leader
    evil-mc
    evil-org
    evil-surround
    expand-region
    git-gutter
    glsl-mode
    js2-mode
    magit
    org-bullets
    origami
    polymode
    powershell
    projectile
    qml-mode
    rjsx-mode
    selectrum
    smart-mode-line
    smartparens
    treemacs
    undo-tree
    vue-mode
    which-key
    winum))

(defun my-packages-installed-p ()
  "Check if all packages in my-packages are installed."
  (cl-every #'package-installed-p my-packages))

(defun install-my-packages ()
  "Install all packages listed in my-packages."
  (unless (my-packages-installed-p)
    (package-refresh-contents)
    (mapc #'package-install my-packages)))

(install-my-packages)

(provide 'my-packages)
