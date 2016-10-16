;;; doom-neotree.el

(require 'all-the-icons)

(defgroup doom-neotree nil
  "Options for doom's neotree theme"
  :group 'doom-themes)

;;
(defface doom-neotree-folder-face '((t (:inherit neo-dir-link-face)))
  "Base face for neotree folder icons. Also see `doom-neotree-open-folder-face' and
`doom-neotree-closed-folder-face'."
  :group 'doom-neotree)

(defface doom-neotree-chevron-face '((t (:inherit neo-dir-link-face)))
  "Face for chevron icons next to folders. See
`doom-neotree-closed-chevron-icon' and `doom-neotree-open-chevron-icon'."
  :group 'doom-neotree)

(defface doom-neotree-dir-face  '((t (:inherit (variable-pitch neo-dir-link-face))))
  "Face for directory labels."
  :group 'doom-neotree)

(defface doom-neotree-file-face '((t (:inherit (variable-pitch neo-file-link-face))))
  "Face for file name labels."
  :group 'doom-neotree)

(defface doom-neotree-root-face '((t (:inherit (variable-pitch neo-root-dir-face))))
  "Face used for root entry (the project name at the top)."
  :group 'doom-neotree)


;;
(defcustom doom-neotree-project-size 1.4
  "What :height to display the project icon at the top at."
  :type 'float
  :group 'doom-neotree)

(defcustom doom-neotree-folder-size 1.05
  "What :height to display the folder icons at."
  :type 'float
  :group 'doom-neotree)

(defcustom doom-neotree-chevron-size 0.8
  "What :height to display the chevron icons at."
  :type 'float
  :group 'doom-neotree)

(defcustom doom-neotree-line-spacing 2
  "Line-spacing for neotree buffer."
  :type 'symbol
  :group 'doom-neotree)

(define-obsolete-variable-alias 'doom-neotree-enable-file-icons 'doom-neotree-file-icons)
(defcustom doom-neotree-file-icons 'simple
  "The style to use for the file icons. Can be nil (disabled), non-nil (for a
diverse iconset), or 'simple, which is closest's to Atom's style as it only
distinguishes text, source, pdfs, images and binary files."
  :type '(choice
          (const :tag "A diverse array of file icons based on file type" t)
          (const :tag "Minimalistic file icons (like Atom's)" 'simple)
          (const :tag "Disable file icons" nil))
  :group 'doom-neotree)

(defcustom doom-neotree-enable-folder-icons t
  "If non-nil, display folder icons next to each file. Different icons are used
depending on whether the folder is a repo, symlink or regular folder."
  :type 'boolean
  :group 'doom-neotree)

(defcustom doom-neotree-enable-open-chevron-icons t
  "If non-nil, display the chevron-down icon next to each expanded folder."
  :type 'boolean
  :group 'doom-neotree)

(defcustom doom-neotree-enable-closed-chevron-icons t
  "If non-nil, display the chevron-right icon next to each collapsed folder."
  :type 'boolean
  :group 'doom-neotree)

(defcustom doom-neotree-enable-variable-pitch nil
  "If non-nil, labels will be use the `doom-neotree-dir-face' and
`doom-neotree-dir-face' faces, which inherit from the `variable-pitch' face."
  :type 'boolean
  :group 'doom-neotree)

(defvar doom--neotree-file-re
  `((code    . ,(concat "\\.\\(p?html?\\|xml\\|ya?ml\\|json\\|tpl\\|conf\\|erb\\|mustache\\|twig\\|ejs\\|haml\\|pug\\|jade\\)$"))
    (media   . ,(concat "\\.\\("
                        "png\\|jpe?g\\|gif\\|tiff\\|svg\\|bmp" ; images
                        "\\|mov\\|avi\\|mp[34]\\|webm"         ; media
                        "\\)$"
                        ))
    (archive . "\\.\\(zip\\|rar\\|7z\\|tar\\(\\.gz\\)?\\)$"))
  "An alist mapping file type to regular expressions, used to determine what
type of icon to display for the file if `doom-neotree-file-icons' is set to
`simple'.")


;;
(defun doom--neotree-no-fringes ()
  "Remove fringes in neotree. They get reset each time you select the neotree
pane and are highlighted incorrectly."
  (set-window-fringes neo-global--window 1 0))

(defun doom--neotree-setup (&rest _)
  (setq line-spacing doom-neotree-line-spacing
        tab-width 1)
  (when (featurep 'hl-line)
    (set (make-local-variable 'hl-line-sticky-flag) t)
    (hl-line-mode +1)))

(defun doom--neotree-folder-icon-for (dir chevron)
  (let* ((path (expand-file-name dir))
         (chevron
          (if chevron
              (all-the-icons-octicon
               (format "chevron-%s" chevron)
               :height doom-neotree-chevron-size
               :v-adjust 0.1
               :face 'doom-neotree-chevron-face)
            spc))
         (icon
          (when doom-neotree-enable-folder-icons
            (all-the-icons-octicon
             (cond ((file-symlink-p path) "file-symlink-directory")
                   ((file-exists-p (format "%s/.git" path)) "file-submodule")
                   ((all-the-icons-dir-is-submodule path) "file-submodule")
                   (t "file-directory"))
             :height doom-neotree-folder-size
             :v-adjust 0
             :face 'doom-neotree-folder-face))))
    (concat chevron "\t" icon)))

(defun doom--neotree-file-icon-for (file-name)
  (cond ((eq doom-neotree-file-icons 'simple)
         (if file-name
             (propertize
               (cond ((string-match-p (cdr (assq 'code doom--neotree-file-re)) file-name)
                      (all-the-icons-octicon "file-code"))
                     ((string-match-p (cdr (assq 'media doom--neotree-file-re)) file-name)
                      (all-the-icons-octicon "file-media"))
                     ((string-match-p (cdr (assq 'archive doom--neotree-file-re)) file-name)
                      (all-the-icons-octicon "file-zip"))
                     ((string= (or (file-name-extension file-name) "") "pdf")
                      (all-the-icons-octicon "file-pdf"))
                     ((file-symlink-p file-name)
                      (all-the-icons-octicon "file-symlink-file"))
                     ((file-executable-p file-name)
                      (all-the-icons-octicon "file-binary"))
                     (t
                      (all-the-icons-octicon "file-text")))
               'face `(:family ,(all-the-icons-octicon-family) :height 1.3)
               'display '(raise 0))
           (all-the-icons-fileicon "default")))
        (t (all-the-icons-icon-for-file file-name))))

(defun doom--neo-insert-fold-symbol (type file-name)
  "Custom hybrid unicode theme with leading whitespace."
  (let ((spc "\t"))
    (or (and (eq type 'open)
             (insert
              (concat spc
                      (doom--neotree-folder-icon-for
                       file-name
                       (if doom-neotree-enable-open-chevron-icons "down"))
                      spc)))
        (and (eq type 'close)
             (insert
              (concat spc
                      (doom--neotree-folder-icon-for
                       file-name
                       (if doom-neotree-enable-closed-chevron-icons "right"))
                      spc)))
        (and (eq type 'leaf)
             (insert
              (concat (when (or doom-neotree-enable-open-chevron-icons
                                doom-neotree-enable-closed-chevron-icons)
                        spc)
                      (when doom-neotree-enable-folder-icons spc)
                      (when doom-neotree-file-icons
                        (concat spc (doom--neotree-file-icon-for file-name)))
                      spc))))))

(defun doom--neo-buffer--insert-root-entry (node)
  "Pretty-print pwd in neotree"
  (let ((project-name (file-name-nondirectory (substring node 0 (1- (length node)))))
        (face (if doom-neotree-enable-variable-pitch
                  'doom-neotree-root-face
                'neo-root-dir-face)))
    (insert
     (concat (propertize " " 'face 'neo-root-dir-face)
             (all-the-icons-octicon "repo"
                                    :height doom-neotree-project-size
                                    :face 'neo-root-dir-face
                                    :v-adjust -0.1)
             (propertize " " 'face 'neo-root-dir-face)
             (propertize (concat project-name "\n") 'face face)))))

(defun doom--neo-buffer--insert-dir-entry (node depth expanded)
  (let ((node-short-name (neo-path--file-short-name node)))
    (insert-char ?\s (* (- depth 1) 2)) ; indent
    (when (memq 'char neo-vc-integration)
      (insert-char ?\s 2))
    ;; Added this line
    (doom--neo-insert-fold-symbol
     (if expanded 'open 'close) node)
    ;;
    (insert-button node-short-name
                   'follow-link t
                   'face (if doom-neotree-enable-variable-pitch
                             'doom-neotree-dir-face
                           'neo-dir-link-face)
                   'neo-full-path node
                   'keymap neotree-dir-button-keymap)
    (neo-buffer--node-list-set nil node)
    (neo-buffer--newline-and-begin)))

(defun doom--neo-buffer--insert-file-entry (node depth)
  (let ((node-short-name (neo-path--file-short-name node))
        (vc (when neo-vc-integration (neo-vc-for-node node))))
    (insert-char ?\s (* (- depth 1) 2)) ; indent
    (when (memq 'char neo-vc-integration)
      (insert-char (car vc))
      (insert-char ?\s))
    ;; Added this line
    (doom--neo-insert-fold-symbol 'leaf node)
    ;;
    (insert-button node-short-name
                   'follow-link t
                   'face (if (memq 'face neo-vc-integration)
                             (cdr vc)
                           (if doom-neotree-enable-variable-pitch
                               'doom-neotree-file-face
                             'neo-file-link-face))
                   'neo-full-path node
                   'keymap neotree-file-button-keymap)
    (neo-buffer--node-list-set nil node)
    (neo-buffer--newline-and-begin)))


;;
(eval-after-load "neotree"
  (lambda ()
    ;; Enable buffer-local hl-line and adjust line-spacing
    (add-hook 'neo-after-create-hook 'doom--neotree-setup)
    ;; Incompatible
    (setq neo-vc-integration nil)
    ;; Remove fringes in Neotree pane
    (advice-add 'neo-global--select-window :after 'doom--neotree-no-fringes)
    ;; Patch neotree to use `doom--neo-insert-fold-symbol'
    (advice-add 'neo-buffer--insert-file-entry :override 'doom--neo-buffer--insert-file-entry)
    (advice-add 'neo-buffer--insert-dir-entry  :override 'doom--neo-buffer--insert-dir-entry)
    ;; Shorter pwd in neotree
    (advice-add 'neo-buffer--insert-root-entry :override 'doom--neo-buffer--insert-root-entry)))

(provide 'doom-neotree)
;;; doom-neotree.el ends here
