# rally-mode
A Rally mode for Emacs


To use:

Eval/load rally-mode.el, then M-x rally-current-iteration, enter username and password and it should display current sprint task info for your user.



Sample config in ~/.emacs.d/init.el:

(add-to-list 'load-path "~/.emacs.d/my-lisp/") ;; rally-mode.el in this directory.
(require 'rally-mode)
(setq rally-user "a-rally-user@example.com")
