(in-package :stumpwm)

(setf *mode-line-timeout* 2)
(setf *time-modeline-string* "%F %H:%M")
(setf *group-format* "%t")

(defvar *mode-line-formatter-list*
  '(("%W") ("^>") ("%d")))

(setf *screen-mode-line-format* *mode-line-formatter-list*)
