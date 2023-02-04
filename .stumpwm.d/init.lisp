#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(in-package :stumpwm)
(setf *default-package* :stumpwm)

(setf *startup-message* nil)

(when *initializing*
  (run-shell-command "nitrogen --restore")
  (toggle-mode-line (current-screen)
                    (current-head)))

(setf *mouse-focus-policy* :sloppy)

(load "~/.stumpwm.d/colours.lisp")
(load "~/.stumpwm.d/keybindings.lisp")
(load "~/.stumpwm.d/modeline.lisp")
(load "~/.stumpwm.d/commands.lisp")
(load "~/.stumpwm.d/theme.lisp")

