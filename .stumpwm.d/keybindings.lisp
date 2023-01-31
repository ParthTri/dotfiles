(in-package :stumpwm)

(which-key-mode)

(stumpwm:set-prefix-key (stumpwm:kbd "s-SPC"))

;; Restart
(define-key *groups-map* (kbd "r") "restart-soft")

;; Helper function
(defun tr-define-key (key command)
  (define-key *top-map* (kbd (concat "s-" key )) command)
  (define-key *root-map* (kbd key) command))

;; Vim navigation
(define-key *top-map* (kbd "s-h") "move-focus left")
(define-key *top-map* (kbd "s-j") "move-focus down")
(define-key *top-map* (kbd "s-k") "move-focus up")
(define-key *top-map* (kbd "s-l") "move-focus right")

;; Tab like cycling
(define-key *top-map* (kbd "s-C-n") "next-in-frame")
(define-key *top-map* (kbd "s-C-p") "prev-in-frame")

;; Frame cycling
(define-key *top-map* (kbd "s-C-k") "fnext")
(define-key *top-map* (kbd "s-C-j") "fprev")

;; Moving Windows
(define-key *top-map* (kbd "s-H") "move-window left")
(define-key *top-map* (kbd "s-J") "move-window down")
(define-key *top-map* (kbd "s-K") "move-window up")
(define-key *top-map* (kbd "s-L") "move-window right")

;; Splitting Frames
(tr-define-key "v" "hsplit")
(tr-define-key "s" "vsplit")
(tr-define-key "x" "remove-split")

;; Managing splits
(setf *resize-increment* 25)

(tr-define-key "f" "fullscreen")

;; Groups
(when *initializing*
  (grename "[WWW]")
  (gnewbg  "[EMACS]")
  (gnewbg  "[TERM]")
  (gnewbg  "[PRIV]")
  )

(clear-window-placement-rules)
(define-key *top-map* (kbd "s-TAB") "gnext")
(define-key *top-map* (kbd "s-ISO_Left_Tab") "gprev")

(define-key *top-map* (kbd "s-1") "gselect 1")
(define-key *top-map* (kbd "s-!") "gmove 1")

(define-key *top-map* (kbd "s-2") "gselect 2")
(define-key *top-map* (kbd "s-@") "gmove 2")

(define-key *top-map* (kbd "s-3") "gselect 3")
(define-key *top-map* (kbd "s-#") "gmove 3")

(define-key *top-map* (kbd "s-4") "gselect 4")
(define-key *top-map* (kbd "s-$") "gmove 4")

(define-key *top-map* (kbd "s-5") "gselect 5")
(define-key *top-map* (kbd "s-%") "gmove 5")

(define-key *top-map* (kbd "s-g") "grouplist")
(define-key *top-map* (kbd "s-G") "gmove")

