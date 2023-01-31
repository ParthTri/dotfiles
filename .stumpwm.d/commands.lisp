(in-package :stumpwm)

(setf *volume-step* "5")

(defun volume (direction)
  (if direction
        (progn
          (concat "exec pactl set-sink-volume 0 +" *volume-step* "%"))
        (progn
          (concat "exec pactl set-sink-volume 0 -" *volume-step* "%"))
        )
  )
(define-key *top-map* (kbd "XF86AudioRaiseVolume") (volume 't))
(define-key *top-map* (kbd "XF86AudioLowerVolume") (volume 'nil))
(define-key *top-map* (kbd "XF86AudioMute") "exec pactl set-sink-mute 0 toggle")
