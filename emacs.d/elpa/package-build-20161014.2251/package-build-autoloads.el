;;; package-build-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "package-build" "package-build.el" (22704 31745
;;;;;;  0 0))
;;; Generated autoloads from package-build.el

(autoload 'package-build-archive "package-build" "\
Build a package archive for package NAME.

\(fn NAME)" t nil)

(autoload 'package-build-package "package-build" "\
Create PACKAGE-NAME with VERSION.

The information in FILE-SPECS is used to gather files from
SOURCE-DIR.

The resulting package will be stored as a .el or .tar file in
TARGET-DIR, depending on whether there are multiple files.

Argument FILE-SPECS is a list of specs for source files, which
should be relative to SOURCE-DIR.  The specs can be wildcards,
and optionally specify different target paths.  They extended
syntax is currently only documented in the MELPA README.  You can
simply pass `package-build-default-files-spec' in most cases.

Returns the archive entry for the package.

\(fn PACKAGE-NAME VERSION FILE-SPECS SOURCE-DIR TARGET-DIR)" nil nil)

(autoload 'package-build-create-recipe "package-build" "\
Create a new recipe for package NAME using FETCHER.

\(fn NAME FETCHER)" t nil)

(autoload 'package-build-current-recipe "package-build" "\
Build archive for the recipe defined in the current buffer.

\(fn)" t nil)

(autoload 'package-build-all "package-build" "\
Build all packages in the `package-build-recipe-alist'.

\(fn)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; package-build-autoloads.el ends here
