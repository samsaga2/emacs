;; enable projectile
(projectile-global-mode)
(setq projectile-indexing-method 'alien)

;; config treemacs
(setq treemacs-git-mode -1
      treemacs-no-png-images t
      treemacs-width 50)

;; key shortcuts
(global-set-key [f5] 'projectile-run-project)
(global-set-key [f9] 'projectile-compile-project)

;; setup some evil leader keys for projectile
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
  "fo" 'projectile-find-other-file
  "pt" 'treemacs)

(provide 'my-projectile)
