;;; package --- Summary

;;; Commentary:

;;; Code:

;; No flashing or alerting
(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)

(scroll-bar-mode -1)    ; Remove bar
(tool-bar-mode -1)      ; Disable the tool bar
(tooltip-mode -1)       ; Disable tooltips
(set-fringe-mode 10)    ; Provide breathing room

(set-face-attribute 'default nil :font "Fira Code" :height 125)

;;; Line numbers
(global-display-line-numbers-mode)
(setq display-line-numbers-type 'visual)
(dolist (mode '(org-mode-hook
		dired-mode-hook
		image-mode-hook
		pdf-view-mode-hook
                term-mode-hook
		treemacs-mode-hook
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

;;; Dashboard
(use-package dashboard
  :ensure t
  :config
  (setq dashboard-agenda-release-buffers t)
  (dashboard-setup-startup-hook)
  (dashboard-refresh-buffer))

(setq dashboard-center-content t)
(setq dashboard-startup-banner "~/.emacs.d/Dragonfly.png")
(with-current-buffer "*dashboard*"
  (emacs-lock-mode 'kill))

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
(use-package kaolin-themes
  :config
  (load-theme 'kaolin-bubblegum t))

(setq kaolin-themes-bold t
      kaolin-themes-italic t
      kaolin-themes-underline t)

;; Modeline
(use-package all-the-icons)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 5)))

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
    "oe" '(eshell :which-key "eshell")))

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

(use-package persp-mode-projectile-bridge
  :ensure t
  :after (persp projectile)
  :init
  (persp-mode-projectile-bridge-mode 1))

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
(setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))

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

;; org-toc
(use-package toc-org
  :ensure t
  :config (add-hook 'org-mode-hook 'toc-org-mode))

;;; Org roam
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/RoamNotes")
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i"    . completion-at-point))
  :config
  (org-roam-setup))

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
	skeletor-project-directory "~/Developer/Projects"
	skeletor-user-directory "~/.dotfiles/.emacs.d/Templates"
	skeletor--project-types nil))

(skeletor-define-template "python-project"
  :title "Python Project"
  :initialise)

(skeletor-define-template "vanilla-js"
  :title "Vanilla JS Project"
  :initialise)

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

;;; Treemacs
(use-package treemacs
  :ensure t
  :config
  (treemacs-resize-icons 14)
  (pt/leader-keys
    "te" '(treemacs :which-key "treemacs")
    ))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp
  :after (treemacs persp-mode)
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

;;;; Programming modes
(add-hook 'prog-mode 'hs-minor-mode)

;;; Python Setup
(use-package elpy
	 :ensure t)

(use-package auto-virtualenv
 :ensure t)

;;; Haskell Setup
(use-package haskell-mode
  :ensure t)

;;;; Web Setup
(use-package web-mode
  :ensure t)

;; html snippets
(use-package emmet-mode
  :ensure t
  :hook ((web-mode . emmet-mode)
	 (html-mode . emmet-mode)
	 (js-jsx-mode . emmet-mode))
  :config
  (setq emmet-move-cursor-between-quotes t))

;;; Autocomplete
(use-package company
  :ensure t
  :hook (prog-mode . company-mode))

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

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-minibuffer-history-key "M-p")
 '(org-agenda-files
   '("/Users/parthtrivedi/org/Exams/Business.org" "/Users/parthtrivedi/org/Exams/English.org" "/Users/parthtrivedi/org/Exams/Exams.org" "/Users/parthtrivedi/org/Exams/Science.org" "/Users/parthtrivedi/org/Pulse/Fencer Analysis.org" "/Users/parthtrivedi/org/Pulse/Landing_page.org" "/Users/parthtrivedi/org/Pulse/Mangement Descriptions.org" "/Users/parthtrivedi/org/Pulse/TODO.org" "/Users/parthtrivedi/org/Research/NixOS.org" "/Users/parthtrivedi/org/Research/Portable OS.org" "/Users/parthtrivedi/org/Research/Remote Development Environment.org" "/Users/parthtrivedi/org/Books_to_read.org" "/Users/parthtrivedi/org/Browser Games.org" "/Users/parthtrivedi/org/Emacs.org" "/Users/parthtrivedi/org/Ideas.org" "/Users/parthtrivedi/org/Keebs.org" "/Users/parthtrivedi/org/Learn.org" "/Users/parthtrivedi/org/Server.org" "/Users/parthtrivedi/org/Subjects.org"))
 '(package-selected-packages
   '(haskell-mode json-mode emmet-mode vterm restclient pdf-tools treemacs-persp persp-mode-projectile-bridge persp-mode treemacs-magit treemacs-projectile treemacs-evil treemacs git-gutter toc-org rainbow-delimiters flycheck rust-mode web-mode auto-virtualenv elpy magit skeletor counsel-projectile projectile hydra general org-bullets evil-collection evil which-key command-log-mode doom-modeline all-the-icons kaolin-themes counsel dashboard use-package)))
