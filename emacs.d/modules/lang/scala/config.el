;;; lang/scala/config.el -*- lexical-binding: t; -*-

(def-package! scala-mode
  :mode "\\.s\\(cala\\|bt\\)$"
  :config (setq scala-indent:align-parameters t))


(def-package! sbt-mode :after scala-mode)


(def-package! ensime
  :commands (ensime ensime-scala-mode-hook)
  :hook (scala-mode . ensime-mode)
  :config
  (add-hook 'ensime-mode-hook #'eldoc-mode)

  (set! :company-backend 'scala-mode '(ensime-company company-yasnippet))

  (setq ensime-startup-snapshot-notification nil
        ensime-startup-notification nil
        ensime-eldoc-hints 'all
        ;; let DOOM handle company setup
        ensime-completion-style nil)

  ;; Fix void-variable imenu-auto-rescan error caused by `ensime--setup-imenu'
  ;; trying to make imenu variables buffer local before imenu is loaded.
  (require 'imenu))

