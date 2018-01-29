[![MELPA](http://melpa.org/packages/evil-terminal-cursor-changer-badge.svg)](http://melpa.org/#/evil-terminal-cursor-changer)

## Introduce ##

evil-terminal-cursor-changer is changing cursor shape and color by evil state for evil-mode.

When running in terminal, It's especially helpful to recognize evil's state.

## Install ##

1. Config melpa: http://melpa.org/#/getting-started

2. M-x package-install RET evil-terminal-cursor-changer RET

3. Add code to your emacs config file:（for example: ~/.emacs）：

For Only terminal

     (unless (display-graphic-p)
             (require 'evil-terminal-cursor-changer))

For All

     (require 'evil-terminal-cursor-changer)

If want change cursor shape type, add below line. This is evil's setting.

     (setq evil-visual-state-cursor '("red" box)); █
     (setq evil-insert-state-cursor '("green" bar)); ⎸
     (setq evil-emacs-state-cursor '("blue" hbar)); _

Now, works in XTerm, Gnome Terminal(Gnome Desktop), iTerm(Mac OS
X), Konsole(KDE Desktop), dumb(etc. mintty), Apple
Terminal.app(restrictive supporting). If using Apple Terminal.app,
must install SIMBL(http://www.culater.net/software/SIMBL/SIMBL.php)
and MouseTerm
plus(https://github.com/saitoha/mouseterm-plus/releases) to use
evil-terminal-cursor-changer. That makes to support VT's DECSCUSR
sequence.
