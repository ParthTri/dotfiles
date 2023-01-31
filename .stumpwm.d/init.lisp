#-quicklisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))

(in-package :stumpwm)
(setf *default-package* :stumpwm)

(setf *startup-message* nil)

(load "~/.stumpwm.d/keybindings.lisp")
(load "~/.stumpwm.d/modeline.lisp")
(load "~/.stumpwm.d/commands.lisp")

