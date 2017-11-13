;;; engine-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "engine-mode" "engine-mode.el" (23030 1774
;;;;;;  0 0))
;;; Generated autoloads from engine-mode.el

(defvar engine-mode nil "\
Non-nil if Engine mode is enabled.
See the `engine-mode' command
for a description of this minor mode.")

(custom-autoload 'engine-mode "engine-mode" nil)

(autoload 'engine-mode "engine-mode" "\
Minor mode for defining and querying search engines through Emacs.

\\{engine-mode-map}

\(fn &optional ARG)" t nil)

(autoload 'defengine "engine-mode" "\
Define a custom search engine.

`engine-name' is a symbol naming the engine.
`search-engine-url' is the url to be queried, with a \"%s\"
standing in for the search term.
The optional keyword argument `docstring' assigns a docstring to
the generated function. A reasonably sensible docstring will be
generated if a custom one isn't provided.
The optional keyword argument `browser` assigns the browser
function to be used when opening the URL.
The optional keyword argument `term-transformation-hook' is a
function that will be applied to the search term before it's
substituted into `search-engine-url'. For example, if we wanted
to always upcase our search terms, we might use:

\(defengine duckduckgo
  \"https://duckduckgo.com/?q=%s\"
  :term-transformation-hook 'upcase)

In this case, searching for \"foobar\" will hit the url
\"https://duckduckgo.com/?q=FOOBAR\".

The optional keyword argument `keybinding' is a string describing
the key to bind the new function.

Keybindings are in the `engine-mode-map', so they're prefixed.

For example, to search Wikipedia, use:

  (defengine wikipedia
    \"http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s\"
    :keybinding \"w\"
    :docstring \"Search Wikipedia!\")

Hitting \"C-x / w\" will be bound to the newly-defined
`engine/search-wikipedia' function.

\(fn ENGINE-NAME SEARCH-ENGINE-URL &key KEYBINDING DOCSTRING (BROWSER \\='engine/browser-function) (TERM-TRANSFORMATION-HOOK \\='identity))" nil t)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; engine-mode-autoloads.el ends here
