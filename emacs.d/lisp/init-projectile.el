;;; package --- Summary

;;; Commentary:

;;; Code:

(provide 'init-projectile)

(require 'projectile)
(require 'helm-projectile)

(add-hook 'ruby-mode-hook 'projectile-mode)
(setq projectile-indexing-method 'alien)
(setq projectile-switch-project-action 'projectile-find-file)
(setq projectile-completion-system 'default)
(setq projectile-enable-caching nil)

(helm-projectile-on)

(set-face-attribute 'helm-source-header nil :foreground "#ffb86c" :height 1.44)

(provide 'init-projectile)
;;; init-projectile.el ends here
