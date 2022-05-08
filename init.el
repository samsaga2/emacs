;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; load my modules
(add-to-list 'load-path (concat user-emacs-directory "modules"))
(require 'my-packages)
(require 'my-ui)
(require 'my-editor)
(require 'my-evil)
(require 'my-projectile)
(require 'my-prog)
