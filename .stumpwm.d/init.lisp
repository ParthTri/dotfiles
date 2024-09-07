;#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(in-package :stumpwm)
(setf *default-package* :stumpwm)

(setf *startup-message* nil)

(when *initializing*
  (toggle-mode-line (current-screen)
                    (current-head))
  (run-shell-command "sxhkd")
  (run-shell-command "picom")
  (run-shell-command "autorandr --change")
  (run-shell-command "nitrogen --restore"))

(setf *mouse-focus-policy* :sloppy)

(setf (getenv "GDK_CORE_DEVICE_EVENTS") "1")

(load "~/.stumpwm.d/colours.lisp")
(load "~/.stumpwm.d/keybindings.lisp")
(load "~/.stumpwm.d/modeline.lisp")
(load "~/.stumpwm.d/commands.lisp")
(load "~/.stumpwm.d/theme.lisp")

