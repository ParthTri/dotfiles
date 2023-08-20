;; #+TITLE: EXWM Config
;; #+AUTHOR: Parth Trivedi
;; #+PROPERTY: header-args:emacs-lisp :tangle ./.emacs.d/desktop.el :comments org


(require 'use-package)
(setq use-package-always-ensure t)

(defun efs/exwm-update-class ()
  (exwm-workspace-rename-buffer exwm-class-name))

(defun efs/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(add-hook 'exwm-init-hook #'efs/after-exwm-init)

;; (defun dw/exwm-init-hook ()
;;   ;; Make workspace 1 be the one where we land at startup
;;   (exwm-workspace-switch-create 1)

;;   ;; Open eshell by default
;;   ;;(eshell)

;;   ;; Launch apps that will run in the background
;;   (efs/run-in-background "nm-applet"))


;; (use-package
  exwm
  :config
  ;; Set the default number of workspaces
  (setq exwm-workspace-number 5)

  ;; When window "class" updates, use it to set the buffer name
  ;; (add-hook 'exwm-update-class-hook #'efs/exwm-update-class)

  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\C-u
          ?\C-h
          ?\M-x
          ?\M-`
          ?\M-&
          ?\M-:
          ?\C-w
          ?\C-\M-j  ;; Buffer list
          ?\C-\ ))  ;; Ctrl+Space

  ;; Ctrl+Q will enable the next key to be sent directly
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  ;; Set up global key bindings.  These always work, no matter the input state!
  ;; Keep in mind that changing this list after EXWM initializes has no effect.
  (setq exwm-input-global-keys
        `(
          ;; Reset to line-mode (C-c C-k switches to char-mode via exwm-input-release-keyboard)
          ([?\s-r] . exwm-reset)

          ;; Move between windows
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)

          ;; Launch applications via shell command
          ([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ;; Switch workspace
          ([?\s-w] . exwm-workspace-switch)

          ;; 's-N': Switch to certain workspace with Super (Win) plus a number key (0 - 9)
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))
  )

;; Xrandr

(require 'exwm-randr)
(exwm-randr-enable)

;; Multimonitor

(setq exwm-randr-workspace-monitor-plist '(2 "DP-1" 3 "eDP-1"))

(setq exwm-workspace-warp-cursor t)
(setq mouse-autoselect-window t
      focus-follows-mouse t)

(defun efs/update-displays ()
  (efs/run-in-background "autorandr --change --force")
  (message "Display config: %s"
           (string-trim (shell-command-to-string "autorandr --current"))))

(add-hook 'exwm-randr-screen-change-hook #'efs/update-displays)
(efs/update-displays)

;; App Launcher

(exwm-input-set-key (kbd "s-SPC") 'counsel-linux-app)
(exwm-input-set-key (kbd "s-f") 'exwm-layout-toggle-fullscreen)

;; Start EXWM

(exwm-enable)
