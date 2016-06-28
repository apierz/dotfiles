(provide 'init-engine)

(require 'engine-mode)
(engine-mode t)

;; engine hotkey: "C-c /"


(defengine duckduckgo
  "https://duckduckgo.com/?q=%s"
  :keybinding "d")



(provide 'init-engine)
