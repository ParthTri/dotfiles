;;; package --- Summary


;;; Code:

;; No flashing or alerting
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)

(scroll-bar-mode -1)    ; Remove bar
(tool-bar-mode -1)      ; Disable the tool bar
(tooltip-mode -1)       ; Disable tooltips
(set-fringe-mode 10)    ; Provide breathing room

(set-face-attribute 'default nil :font "Fira Code" :height 115)

;;; Line numbers
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'visual)
(dolist (mode '(org-mode-hook
		org-agenda-mode-hook
		dired-mode-hook
		image-mode-hook
		pdf-view-mode-hook
                term-mode-hook
		neotree-mode-hook
                eshell-mode-hook
		vterm-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
 (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
   (package-install 'use-package))

;; Write backups to ~/.emacs.d/backup/
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      20 ; how many of the newest versions to keep
      kept-old-versions      5) ; and how many of the old

(require 'use-package)
(setq use-package-always-ensure t)

;; Get counsel
(use-package counsel
  :ensure t
  :config (counsel-mode))

(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-k") 'kill-this-buffer)

;; Setup Ivy
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (ivy-mode 1))

;; Get theme
(use-package atom-one-dark-theme
  :ensure t
  :config
  (load-theme 'atom-one-dark t)
  (setq atom-one-dark-theme-force-faces-for-mode t))

;; Modeline
(use-package all-the-icons
  :ensure t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 5)
  (display-time-mode 't))

;;; Command log mode
(use-package command-log-mode)

;;; Which key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle 0.3))

;;; Keybindings
(use-package general
  :ensure t
  :config
  (general-create-definer pt/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (pt/leader-keys
    "SPC" '(find-file :which-key "files")
    "RET" '(bookmark-jump :which-key "bookmarks"))
  
  (pt/leader-keys
    "b" '(:ignore b :which-key "buffer")
    "bk" '(kill-this-buffer :which-key "kill")
    "bi" '(ibuffer :which-key "ibuffer")
    "bb" '(switch-to-buffer :which-key "switch"))
  
  (pt/leader-keys
   "t"  '(:ignore t :which-key "toggles")
   "tt" '(counsel-load-theme :which-key "choose theme"))

  (pt/leader-keys
    "w" '(:ignore w :which-key "window")
    "ws" '(evil-window-split :which-key "horizontal split")
    "wv" '(evil-window-vsplit :which-key "vertical split")
    "wd" '(evil-window-delete :which-key "delete")
    "wr" '(evil-window-rotate-upwards :which-key "rotate")
    "wh" '(evil-window-left :which-key "left")
    "wj" '(evil-window-down :which-key "down")
    "wk" '(evil-window-up :which-key "up")
    "wl" '(evil-window-right :which-key "right"))
  (pt/leader-keys
    "o" '(:ignore o :which-key "open")
    "oe" '(eshell :which-key "eshell")
    "oa" '(org-agenda :which-key "agenda")
    "oc" '(org-capture :which-key "capture")))

(global-set-key (kbd "M-/") 'comment-or-uncomment-region)

;;; Workspaces
(use-package persp-mode
  :ensure t
  :config
  (persp-mode)
  (pt/leader-keys
    "k" '(:ignore k :which-key "workspaces")
    "ka" '(persp-add-buffer :which-key "add")
    "ks" '(persp-switch :which-key "switch")
    "kr" '(persp-remove-buffer :whick-key "remove")
    "kb" '(persp-switch-to-buffer :which-key "buffer")
    "kk" '(persp-kill :which-key "kill")
  ))

;;; Evil
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))


;;; Org configuration
(setq org-directory "~/org/")

(defun pt/org-mode-setup ()
  (org-indent-mode)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(use-package org
  :hook (org-mode . pt/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t))

(setq org-hide-emphasis-markers t)
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
                         '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

;; Make sure org-indent face is available
(require 'org-indent)

;; Org headings
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :height 1.5))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.4))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.3))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.2))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.1)))))

;; org capture
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq gtd-file "~/org/gtd.org")
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline gtd-file "Tasks")
	 "** TODO %?\n %i\n %a")
	("s" "School" entry (file+headline gtd-file "Projects")
	 "** TODO [/]%?\n#+COOKIE_DATA:todo\n*** NEXT Module(s)\n*** NEXT TMA(s)\n*** NEXT Assessment\n")
	("p" "Project" entry (file+headline gtd-file "Projects")
	 "** %? [/]\n#+COOKIE_DATA:todo\n %i\n")
	("l" "Something for Later" entry (file+headline gtd-file "Later")
	 "** %?\n %i\n")
	("i" "Idea" entry (file+headline "~/org/Ideas.org" "General")
	 "** %?\n %i\n ")))

;; org refile
(setq org-refile-targets
      '(("~/org/gtd.org" :maxlevel . 1)
        ("~/org/Ideas.org" :maxlevel . 1)
	("~/org/done.archive.org" :maxlevel . 1)))

;; org tags
(setq org-tag-alist '((:startgroup)
		      ("@work" . ?W)
		      ("@home" . ?H)
		      (:endgroup)
		      ("work" . ?w)
		      ("privy" . ?p)
		      ("school" . ?s)
		      ("dev" . ?d)))

;; org-toc
(use-package toc-org
  :ensure t
  :config (add-hook 'org-mode-hook 'toc-org-mode))

;;; Org Export
;; iCal
(setq org-icalendar-use-scheduled '(event-if-todo-not-done))

;; html
(setq org-html-head "<link rel='stylesheet' type='text/css' href='~/.dotfiles/.emacs.d/html_export.css' />")

;;; Org Agenda
(setq org-agenda-files '("~/org/gtd.org"
                         "~/Documents/School Work/Subjects.org"))

(setq org-todo-keywords
      '((sequencep "TODO(t)" "ONGOING(o)" "NEXT(n)" "|" "DONE(d/!)")
        (sequencep "WAITING(w@/!)" "|" "CANCELLED(c@/!)" "PAUSED(p@/!)" "MEETING")))

(setq org-todo-keyword-faces
      '(("TODO" :foreground "Purple" :weight bold )
        ("ONGOING" :foreground "Orange" :weight bold)
	("NEXT" :foreground "DeepSkyBlue" :weight bold)
        ("DONE" :foreground "SeaGreen3" :weight bold)
        ("WAITING" :foreground "DeepSkyBlue" :weight bold)
        ("CANCELLED" :foreground "Red" :weight bold)
        ("PAUSED" :foreground "OrangeRed" :weight bold)
        ("MEETING" :foreground "forest green" :weight bold)))

(setq org-agenda-dim-blocked-tasks nil)
(setq org-agenda-custom-commands
      '(("n" "All"
	 ((agenda "" nil)
	  (todo "ONGOING"
		((org-agenda-overriding-header "Ongoing Tasks")))
	  (todo "NEXT"
		((org-agenda-overriding-header "Next Tasks")))
	  (todo "WAITING"
		((org-agenda-overriding-header "Waiting On"))))
	 nil)
	))
      
;;; Org mobile
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-mobile-inbox-for-pull "~/org/flagged.org")
(setq org-mobile-files (list "~/org/Ideas.org"
			 "~/org/Books.org"
			 "~/org/gtd.org"
			 "~/org/Learn.org"))

;;; Org roam
;; (use-package org-roam
;;   :ensure t
;;   :init
;;   (setq org-roam-v2-ack t)
;;   :custom
;;   (org-roam-directory "~/RoamNotes")
;;   (org-roam-completion-everywhere t)
;;   :bind (("C-c n l" . org-roam-buffer-toggle)
;;          ("C-c n f" . org-roam-node-find)
;;          ("C-c n i" . org-roam-node-insert)
;;          :map org-mode-map
;;          ("C-M-i"    . completion-at-point))
;;   :config
;;   (org-roam-setup))


;;; Hydra
(use-package hydra)
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(pt/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

;;; Projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action #'projectile-dired)

  (pt/leader-keys
    "p" '(:ignore p :which-key "projects")
    "pp" '(projectile-switch-project :which-key "switch to project")
    "pt" '(projectile-test-project :which-key "test project")))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;;; Skeletor
(use-package skeletor
  :config
  (setq skeletor-completing-read-function 'ivy-completing-read
	skeletor-project-directory "~/Projects"
	skeletor-user-directory "~/.dotfiles/.emacs.d/Templates"
	skeletor--project-types nil))

(skeletor-define-template "python-project"
  :title "Python Project"
  :after-creation
  (lambda (dir)
    (skeletor-async-shell-command "python3 -m venv venv")
    (vterm)
    (vterm-send-string (format "cd %s \n" dir))
    (rename-buffer skeletor-project-name)
    )
  :initialise)

(skeletor-define-template "vanilla-js"
  :title "Vanilla JS Project"
  :initialise)

(skeletor-define-template "react-project"
  :title "React.js Project"
  :no-license? t
  :after-creation
  (lambda (dir)
    (skeletor-async-shell-command "create-react-app $PWD"))
  :initialise
  )

(pt/leader-keys
  "pc" '(skeletor-create-project :which-key "create project")
  "pC" '(skeletor-create-project-at :which-key "create project at")
  )

;;; Magit
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (pt/leader-keys
    "g" '(:ignore g :which-key "git")
    "gs" '(magit-stage-file :which-key "stage file")
    "gS" '(magit-stage :which-key "stage all")
    "gc" '(magit-commit :which-key "commit")
    "gg" '(magit-status :which-key "status")))

;;; Git Gutter
(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode t))

;;; Neotree
(use-package neotree
  :ensure t
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (pt/leader-keys
   "te" '(neotree-toggle :which-key "neotree")))

(use-package persp-mode-projectile-bridge
  :ensure t
  :after (persp projectile))
(persp-mode-projectile-bridge-mode t)

;;;; Programming modes
(add-hook 'prog-mode-hook 'hs-minor-mode)

;; LSP mode
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (prog-mode . lsp-mode)
  :init
  (setq lsp-keymap-prefix "C-c L")
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-prefer-capf t))

;;; Python Setup
(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

(use-package pyvenv
  :config
  (pyvenv-mode 1))

;;; Lua Setup
(use-package lua-mode
  :ensure t)

;;; Nim support
(use-package nim-mode
  :ensure t)

;;;; Web Setup
(use-package web-mode
  :ensure t)

;;; rjsx
(use-package rjsx-mode
  :ensure t
  :mode "\\.js\\'")

(use-package prettier-js
  :ensure t
  :after (rjsx-mode)
  :hook (rjsx-mode . prettier-js-mode))

;; html snippets
(use-package emmet-mode
  :ensure t
  :hook ((web-mode . emmet-mode)
	 (rjsx-mode . emmet-mode))
  :config
  (setq emmet-move-cursor-between-quotes t))

;;; Autocomplete
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0))

(use-package company-box
  :ensure t
  :after (company-mode)
  :hook (company-mode . company-box-mode))

;;; Centaur Tabs
(use-package centaur-tabs
  :ensure t
  :bind (("C-c l" . centaur-tabs-forward)
	 ("C-c h" . centaur-tabs-backward)
	 ("C-c k" . centaur-tabs-move-current-tab-to-right)
	 ("C-c j" . centaur-tabs-move-current-tab-to-left))
  :config
  (setq centaur-tabs-set-bar 'over
	centaur-tabs-set-icons t
	centaur-tabs-gray-out-icons 'buffer
	centaur-tabs-height 24
	centaur-tabs-set-modified-marker t
	centaur-tabs-modified-marker "•")
  (centaur-tabs-group-by-projectile-project)
  (centaur-tabs-headline-match)
  (centaur-tabs-mode t))

(pt/leader-keys
  "tc" '(centaur-tabs-mode :which-key "tabs"))

;;; Syntax Checking
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

;;; Pdf tools
(use-package pdf-tools
  :ensure t
  :init (pdf-tools-install))

;;; Rainbow deliminators
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package restclient
  :ensure t)

(use-package vterm
  :ensure t
  :config
  (pt/leader-keys
    "ot" '(vterm :which-key "terminal")))

(use-package json-mode)

(use-package dockerfile-mode
  :ensure t)

;;; Init.el ends here
