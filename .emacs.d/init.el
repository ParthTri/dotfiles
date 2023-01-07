;; No flashing or alerting

(setq inhibit-startup-message t)
(setq ring-bell-function 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Remove bar

(scroll-bar-mode -1)    ; Remove bar
(tool-bar-mode -1)      ; Disable the tool bar
(tooltip-mode -1)       ; Disable tooltips
(set-fringe-mode 10)    ; Provide breathing room
(menu-bar-mode 0)       ; Remove Menu Bar

;; Font

(set-face-attribute 'default nil :font "iaWriter Duo S" :height 95)

;; Line Numbers

(global-display-line-numbers-mode)
(setq display-line-numbers-type 'visual)
(dolist (mode '(org-mode-hook
                org-agenda-mode-hook
                dired-mode-hook
                image-mode-hook
                pdf-view-mode-hook
                term-mode-hook ))
                (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Make ESC quit prompts

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Move to trash instead of delete

(setq delete-by-moving-to-trash t)

;; Tab

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)

;; Use package

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
(require 'use-package)
(setq use-package-always-ensure t)

;; Backups
;; Write backups to ~/.emacs.d/backup/

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying      t  ; Don't de-link hard links
      version-control        t  ; Use version numbers on backups
      delete-old-versions    t  ; Automatically delete excess backups:
      kept-new-versions      20 ; how many of the newest versions to keep
      kept-old-versions      5) ; and how many of the old

;; Theme

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-tokyo-night t)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t)
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Modeline

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 5)
  (display-time-mode 't))

;; Icons

(use-package all-the-icons
  :if (display-graphic-p))

;; Which Key

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle 0.3))

;; Keybindings

(use-package general
  :ensure t
  :config
  (general-create-definer pt/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC"))

(pt/leader-keys
  "SPC" '(find-file :which-key "files")
  "RET" '(list-bookmarks :which-key "bookmarks"))

(pt/leader-keys
  "b" '(:ignore b :which-key "buffer")
  "bk" '(kill-this-buffer :which-key "kill")
  "bi" '(ibuffer :which-key "ibuffer")
  "bb" '(switch-to-buffer :which-key "switch"))

(pt/leader-keys
  "t"  '(:ignore t :which-key "toggles")
  "tv" '(visual-line-mode :which-key "visual line mode")
  "ts" '(hydra-text-scale/body :which-key "scale text"))

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
  "oe" '(eshell :which-key "eshell"))

(pt/leader-keys
  "O" '(:ignore O :which-key "Org")
  "Oa" '(org-agenda :which-key "agenda")
  "Oc" '(org-capture :which-key "capture"))

(global-set-key (kbd "M-/") 'comment-or-uncomment-region)

;; Ivy

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

;; Counsel

(use-package counsel
  :ensure t
  :config (counsel-mode))

(global-set-key (kbd "M-x") 'counsel-M-x)

;; Hydra

(use-package hydra)
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

;; Evil

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
  (define-key evil-normal-state-map (kbd "?") 'replace-regexp)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Persp

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

;; Company

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

;; Pdf Tools

(use-package pdf-tools
  :ensure t
  :init (pdf-tools-install))

;; Transparency

(setq transparent 'nil)

(defun set-transparency (value)
  "Set transparency based on value passed"
  (set-frame-parameter (selected-frame) 'alpha `(,value 100))
  (add-to-list 'default-frame-alist `(alpha ,value 100)))

(defun toggle-transparency ()
  "Toggle transparency function"
  (interactive)
  (if transparent
      (progn
        (set-transparency 100)
        (setq transparent 'nil))

    (progn
      (set-transparency 85)
      (setq transparent 't))
    ))

(pt/leader-keys
  "tt" '(toggle-transparency :which-key "transparency"))

;; Moving Frames

(global-set-key (kbd "<prior>") 'ns-next-frame)
(global-set-key (kbd "<next>") 'ns-prev-frame)

;; Org Capture Todo

(fset 'open-org-capture-todo
   (kmacro-lambda-form [?  ?o ?c ?t] 0 "%d"))

;; Rainbow Delimiters

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Org Configuration

(setq org-directory "~/Notes/")

(defun pt/org-mode-setup ()
  (org-indent-mode)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  ;; (flyspell-mode)
  (setq evil-auto-indent nil))

(use-package org
  :hook ((org-mode . pt/org-mode-setup))
  :bind (:map org-mode-map
              ("C-C e" . org-mobile-push)
              ("C-c i" . org-mobile-pull)
              ("C-c l" . latex-frag)
              ("C-c L" . latex-frag-mult)
              ("C-c R" . org-table-sort-lines)
              ("C-c [" . org-reftex-citation))

  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t)
  (setq org-image-actual-width nil))

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

(setq org-duration-format (quote h:mm))

;; Org Citations

(use-package org-ref
  :ensure t
  :after org)

;; Single line

(fset 'latex-frag
      (kmacro-lambda-form [?i ?\\ ?b ?e ?g ?i ?n ?\{ ?\} escape ?i ?e ?q ?a backspace ?u ?a ?t ?i ?o ?n escape ?y ?y ?p ?w ?c ?w ?e ?n ?d escape ?O escape ?\s-s] 0 "%d"))

;; Multiline 

(fset 'latex-frag-mult
   (kmacro-lambda-form [?i ?\\ ?b ?e ?g ?i ?n ?\{ ?e ?q ?u ?a ?t ?i ?o ?n ?\} escape ?y ?y ?p ?l ?w ?w ?c ?w ?s ?p ?l ?i ?t escape ?y ?y ?p ?w ?c ?w ?e ?n ?d escape ?k ?k ?y ?y ?j ?j ?p ?w ?c ?w ?e ?n ?d escape ?k ?O escape] 0 "%d"))

;; Custom Faces

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

;; Agenda

;; (setq org-agenda-files '("~/org/gtd.org"))

;; Capture

(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq gtd-file "~/org/gtd.org")
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline gtd-file "Tasks")
         "** TODO %?\n %i\n")
        ("s" "School" entry (file+headline gtd-file "Projects")
         "** TODO [/]%?\n")
        ("p" "Project" entry (file+headline gtd-file "Projects")
         "** %? [/]\n#+COOKIE_DATA:todo\n %i\n")
        ("l" "Something for Later" entry (file+headline gtd-file "Later")
         "** %?\n %i\n")
        ("i" "Idea" entry (file+headline "~/Notes/Ideas.org" "Other")
         "** %?\n %i\n ")
        ("B" "Book" entry (file+headline "~/Notes/Books.org" "Other")
         "** TODO %?\n")))

;; Refile

(setq org-refile-targets
      '(("~/Notes/gtd.org" :maxlevel . 1)
        ("~/Notes/Ideas.org" :maxlevel . 1)
        ("~/Notes/done.archive.org" :maxlevel . 1)
        ("~/Notes/Books.org" :maxlevel . 1)))

;; Tags

(setq org-tag-alist '((:startgroup)
                      ("@work" . ?W)
                      ("@home" . ?H)
                      (:endgroup)
                      ("work" . ?w)
                      ("privy" . ?p)
                      ("learn" . ?l)
                      ("school" . ?s)
                      ("dev" . ?d)
                      ("paid" . ?P)
                      ("volunteer" . ?V)
                      ("crypt" . ?c)))

;; Keywords

(setq org-todo-keywords
      '((sequencep "TODO(t)" "ONGOING(o)" "NEXT(n)" "|" "DONE(d/!)")
        (sequencep "WAITING(w@/!)" "|" "CANCELLED(c@/!)" "PAUSED(p@/!)" "MEETING")))

;; Keyword Faces

(setq org-todo-keyword-faces
      '(("TODO" :foreground "Purple" :weight bold )
        ("ONGOING" :foreground "Orange" :weight bold)
        ("NEXT" :foreground "DeepSkyBlue" :weight bold)
        ("DONE" :foreground "SeaGreen3" :weight bold)
        ("WAITING" :foreground "DeepSkyBlue" :weight bold)
        ("CANCELLED" :foreground "Red" :weight bold)
        ("PAUSED" :foreground "OrangeRed" :weight bold)
        ("MEETING" :foreground "forest green" :weight bold)))

;; Views

(setq org-agenda-dim-blocked-tasks nil)

;; All

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

;; Mobile

(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-mobile-inbox-for-pull "~/org/flagged.org")
(setq org-mobile-files (list "~/org/Ideas.org"
                             "~/org/Books.org"
                             "~/org/gtd.org"
                             "~/org/Learn.org"
                             "~/org/Shows to watch.org"))

;; Crypt

(use-package org-crypt
  :ensure nil
  :after org
  :bind (:map org-mode-map
              ("C-c d" . org-decrypt-entry))
  :config
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt"))
  :custom
  (setq org-crypt-key "0x577FBF62"))

;; Epa

(use-package epa
  :ensure t
  :config
  (custom-set-variables '(epa-gpg-program "/usr/local/bin/gpg"))
  (epa-file-enable))

;; Tangle on save

(defun pt/org-babel-tangle-config ()
    (when (string-equal (buffer-file-name)
                        (expand-file-name "~/.dotfiles/Emacs.org"))
      ;; Dynamic scoping to the rescue
      (let ((org-confirm-babel-evaluate nil))
        (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'pt/org-babel-tangle-config)))

;; Roam

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/Wiki")
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "${slug}-%<%H%M%d%m%Y>.org" "#+title: ${title}\n")
      :unnarrowed t)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i"    . completion-at-point))
  :config
  (org-roam-setup))

;; Roam UI

(use-package org-roam-ui
  :ensure t
  :after org-roam
  :bind (("C-c n u" . org-roam-ui-mode))
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; Journal

(use-package org-journal
  :ensure t
  :init
  (pt/leader-keys
    "Oj" '(org-journal-new-entry :which-key "journal"))
  :config
  (setq org-journal-dir "~/journal/"
        org-journal-date-format "%A, %d %B %Y"))

;; toc

(use-package toc-org
  :ensure t
  :config (add-hook 'org-mode-hook 'toc-org-mode))

;; iCal

(setq org-icalendar-use-scheduled '(event-if-todo-not-done))

;; Html

(setq org-html-head "<link rel='stylesheet' type='text/css' href='~/.dotfiles/.emacs.d/html_export.css' />")

;; Latex

(setq org-latex-toc-command "\\tableofcontents \\clearpage")
(setq org-latex-packages-alist '(("margin=1.7cm" "geometry" nil)))

(setq org-latex-listings t)
(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))
(add-to-list 'org-latex-packages-alist '("" "tabularx"))
(add-to-list 'org-latex-packages-alist '("" "longtable"))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
                 "\\documentclass{article}
             [NO-DEFAULT-PACKAGES]
             [PACKAGES]
             [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; Presentations

(use-package org-tree-slide
  :ensure t
  :after org
  :config
  (setq org-tree-slide-slide-in-effect nil))

(pt/leader-keys
  "Op" '(org-tree-slide-mode :which-key "Present"))

;; Temporary Buffer

(defun create-tmp-org ()
  "Create a temporary org buffer"
  (interactive)
  (create-file-buffer "tmp.org")
  (persp-add-buffer "tmp.org")
  (switch-to-buffer "tmp.org")
  (org-mode))

(pt/leader-keys
  "Oo" '(create-tmp-org :which-key "tmp org"))

;; Writeroom

(use-package writeroom-mode
  :ensure t
  :config
  (setq writeroom-width 130)
  (pt/leader-keys
    "tw" '(writeroom-mode :which-key "Writeroom")))

;; Magit

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

;; Git Gutter

(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode t))

(pt/leader-keys
  "tg" '(git-gutter-mode :which-key "gutter"))

;; Projectile

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
    "pt" '(projectile-test-project :which-key "test project")
    "pf" '(projectile-find-file :which-key "find file")))

(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package persp-mode-projectile-bridge
  :ensure t
  :after (persp projectile))

(persp-mode-projectile-bridge-mode)

;; Syntax Checking

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode))

;; Python

(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

(use-package pyvenv
  :config
  (pyvenv-mode 1))

(use-package lsp-jedi
  :ensure t
  :config
  (with-eval-after-load "lsp-mode"
    (add-to-list 'lsp-disabled-clients 'pyls)
    (add-to-list 'lsp-enabled-clients 'jedi)))

;; Go

(use-package go-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

;; Web

(use-package web-mode
  :ensure t)

;; Emmet

(use-package emmet-mode
  :ensure t
  :hook ((web-mode . emmet-mode)
         (js-mode . emmet-mode))
  :config
  (setq emmet-move-cursor-between-quotes t))

;; JSX

(use-package rjsx-mode
  :mode ("\\.js\\'"
         "\\.jsx\\'")
  :config
  (setq js2-mode-show-parse-errors nil
        js2-mode-show-strict-warnings nil
        js2-basic-offset 2
        js-indent-level 2))

;; LSP

(use-package lsp-mode
  :ensure t
  :commands lsp
  :diminish lsp-mode
  :hook ((python-mode . lsp)
         ((js2-mode rjsx-mode) . lsp))
  :init
  (setq lsp-keymap-prefix "s-l")
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-prefer-capf t)
  (setq lsp-auto-configure t
        lsp-auto-guess-root t
        ;; don't set flymake or lsp-ui so the default linter doesn't get trampled
        lsp-diagnostic-package :none))

;; Lsp UI

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

;; Terminal

(use-package vterm
  :ensure t
  :config
  (pt/leader-keys
    "oT" '(vterm :which-key "terminal"))

;; Toggle

(use-package vterm-toggle
  :ensure t
  :config
  (pt/leader-keys
    "ot" '(vterm-toggle :which-key "terminal")))

;; Comments

(use-package hl-todo
  :ensure t
  :hook (prog-mode))

;; Code Folding

(add-hook 'prog-mode-hook (lambda () (hs-minor-mode 1)))

;; Ledger

(use-package ledger-mode
  :ensure t
  :mode ("\\.journal\\'" "\\.ledger.*\\'"))

(setq ledger-binary-path "hledger")
(setq ledger-mode-should-check-version nil)
(add-to-list 'auto-mode-alist '("\\.\\(h?ledger\\|journal\\|j\\)$" . ledger-mode))

(defvar ledger-report-balance
  (list "bal" (concat ledger-binary-path " -f %(ledger-file) bal")))

(defvar ledger-report-reg
  (list "reg" (concat ledger-binary-path " -f %(ledger-file) reg")))

(defvar ledger-report-payee
  (list "payee" (concat ledger-binary-path " -f %(ledger-file) reg @%(payee)")))

(defvar ledger-report-account
  (list "account" (concat ledger-binary-path " -f %(ledger-file) reg %(account)")))

(setq ledger-reports
      (list ledger-report-balance
            ledger-report-reg
            ledger-report-payee
            ledger-report-account))

;; Centered Window

(use-package centered-window
  :ensure t
  :config
  (pt/leader-keys
    "tc" '(centered-window-mode :which-key "center"))
  (setq cwm-centered-window-width 140))

;; Mu4e

(require 'mu4e)
(setq mu4e-maildir (expand-file-name "~/.mail"))


(setq mu4e-view-show-addresses t
      message-kill-buffer-on-exit t
      mu4e-context-policy 'pick-first
      mu4e-confirm-quit nil)

(pt/leader-keys
  "m" '(mu4e :which-key "Mail"))

;; Set how email is to be sent
(setq send-mail-function (quote smtpmail-send-it))


(setq mu4e-maildir-shortcuts
      '(("/[Gmail]/Sent Mail" . ?s)
        ("/[Gmail]/Trash"     . ?t)
        ("/[Gmail]/Drafts"    . ?d)
        ("/[Gmail]/All Mail"  . ?a)))

;; Accounts

(setq mu4e-user-mail-address-list '("superparthman@gmail.com")

(setq mu4e-contexts
      (list
       ;; Personal account
       (make-mu4e-context
        :name "Personal"
        :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
        :vars '((user-mail-address . "superparthman@gmail.com")
                (user-full-name    . "Parth Trivedi")
                (smtpmail-smtp-server  . "smtp.gmail.com")
                (smtpmail-smtp-service . 465)
                (smtpmail-stream-type  . ssl)
                (mu4e-drafts-folder  . "/Personal/[Gmail]/Drafts")
                (mu4e-sent-folder  . "/Personal/[Gmail]/Sent Mail")
                (mu4e-refile-folder  . "/Personal/[Gmail]/All Mail")
                (mu4e-trash-folder  . "/Personal/[Gmail]/Trash")))
       (make-mu4e-context
        :name "Work"
        :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
        :vars '((user-mail-address . "parthtrivedi.co@gmail.com")
                (user-full-name    . "Parth Trivedi")
                (smtpmail-smtp-server  . "smtp.gmail.com")
                (smtpmail-smtp-service . 465)
                (smtpmail-stream-type  . ssl)
                (mu4e-drafts-folder  . "/Work/[Gmail]/Drafts")
                (mu4e-sent-folder  . "/Work/[Gmail]/Sent Mail")
                (mu4e-refile-folder  . "/Work/[Gmail]/All Mail")
                (mu4e-trash-folder  . "/Work/[Gmail]/Trash")))
        ))

