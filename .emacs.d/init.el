;; #+TITLE: Emacs Configuration
;; #+AUTHOR: Parth Trivedi
;; #+DESCRIPTION: My Emacs Configuration that I use on a daily basis for writing
;; #+PROPERTY: header-args:emacs-lisp :tangle ./.emacs.d/init.el :comments org


(setq byte-compile-warnings '(cl-functions))
;; (setq debug-on-error t)

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

(defun fontify-frame (frame)
  (set-frame-parameter frame 'font "JetBrainsMonoNL Nerd Font Mono"))

;; Fontify current frame
(fontify-frame nil)

;; Fontify any future frames
(push 'fontify-frame after-make-frame-functions)

;; Line Numbers

(global-display-line-numbers-mode)
(setq display-line-numbers-type 'visual)
(dolist (mode '(org-mode-hook
                org-agenda-mode-hook
                dired-mode-hook
                image-mode-hook
                pdf-view-mode-hook
                vterm-mode-hook
                term-mode-hook ))
                (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Make ESC quit prompts

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Move to trash instead of delete

(setq delete-by-moving-to-trash t)

;; Tab

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default evil-shift-width tab-width)
(setq indent-line-function 'insert-tab)

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

;; Straight

(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(straight-use-package 'org)

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
  (load-theme 'doom-challenger-deep t)
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
  (setq doom-modeline-height 2)
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
  "RET" '(counsel-bookmark :which-key "bookmarks"))

(pt/leader-keys
  "b" '(:ignore b :which-key "buffer")
  "bk" '(kill-this-buffer :which-key "kill")
  "bi" '(ibuffer :which-key "ibuffer")
  "bb" '(switch-to-buffer :which-key "switch")
  "br" '(revert-buffer-quick :which-key "revert"))

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
  "oE" '(eshell :which-key "eshell"))

(pt/leader-keys
  "o" '(:ignore O :which-key "")
  "oa" '(org-agenda :which-key "agenda")
  "oc" '(org-capture :which-key "capture"))

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
  :config (counsel-mode)
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only))


(global-set-key (kbd "M-x") 'counsel-M-x)

;; Hydra

(use-package hydra)
(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

;; Emacs Keybindings Cheat Sheet

(defun open-keybind-cheat-sheet ()
  "Open the keybinding cheat sheet in another window"
  (interactive)
  (find-file-other-window "~/Downloads/Cheatsheet-emacs.pdf"))
(global-set-key (kbd "C-h C-k") 'open-keybind-cheat-sheet)

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

  ;; exit insert mode by pressing jj quickly
  (define-key evil-insert-state-map (kbd "C-;") 'evil-normal-state)

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
  (setq persp-autokill-buffer-on-remove t)
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

;; Save View

(use-package pdf-view-restore
  :ensure t
  :after pdf-view
  :hook (pdf-view-mode . pdf-view-restore-mode))

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

;; Notifications

(use-package alert
  :commands alert
  :config
  (setq alert-default-style 'notifications))

;; Neotree

(use-package neotree
  :ensure t
  :init
  (setq neo-smart-open t)
  (setq neo-theme 'icons))

(pt/leader-keys
  "oe" '(neotree-toggle :which-key "Neotree"))

;; Auto Update Packages

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;; Calendar

(use-package calfw
  :ensure t
  :config
  (setq calendar-week-start-day 1))

(use-package calfw-org
  :ensure t
  :custom
  (setq cfw:org-agenda-schedule-args '(:timestamp))
  (defalias 'ca 'cfw:open-org-calendar))

(use-package calfw-ical
  :ensure t)

(defun my-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Blue")  ; orgmode source
   )))

(pt/leader-keys
  "oC" '(my-open-calendar :which-key "Calendar"))

;; Calendar Capture

(setq cfw:org-capture-template '
          ("c" "Calendar Event" entry (file calendar-file)
           "* %?\n"))

;; Dashboard

(use-package dashboard
  :ensure t
  :config
  (setq dashboard-agenda-release-buffers t
        dashboard-center-content t)
  (dashboard-setup-startup-hook)
  (dashboard-refresh-buffer))

(setq initial-buffer-choice 'dashboard-open)

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

;; Org Habits

(setq org-modules '(org-habit))
;; (org-load-modules-maybe t)

;; Org Citations

(require 'oc)

(use-package citar
  :ensure t
  :after org
  :custom
  (org-cite-global-bibliography '("/home/parth/Zotero/main.bib"))
  (citar-bibliography org-cite-global-bibliography)
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  :general
  (:keymaps 'org-mode-map
            :prefix "C-c b"
            "b" '(citar-insert-citation :wk "Insert citation")
            "r" '(citar-insert-reference :wk "Insert reference")
            "o" '(citar-open-notes :wk "Open note"))
  )

;; Org Mime

(use-package org-mime
  :ensure t)

;; (remove-hook 'org-mime-html-hook
;;           (lambda ()
;;             (org-mime-change-element-style
;;              "outline-2" ("color: red;"))))

;; Org Pomodoro

(use-package org-pomodoro
  :ensure t
  :commands (org-pomodoro)
  :config
  (setq
   alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil)))
   org-pomodoro-length 25
   org-pomodoro-short-break-length 5
   ))

(pt/leader-keys
  "P" '(org-pomodoro :which-key "pomodoro"))

(defun pt/org-pomodoro-time ()
  "Return the remaining pomodoro time"
  (if (org-pomodoro-active-p)
      (cl-case org-pomodoro-state
        (:pomodoro
         (format "Pomo: %d minutes" (/ (org-pomodoro-remaining-seconds) 60)))
        (:short-break
         (format "Short break time: %d minutes" (/ (org-pomodoro-remaining-seconds) 60)))
        (:long-break
         (format "Long break time: %d minutes" (/ (org-pomodoro-remaining-seconds) 60)))
        (:overtime
         (format "Overtime! %d minutes" (/ (org-pomodoro-remaining-seconds) 60))))
    "No active pomo"))

;; Single line

(fset 'latex-frag
      (kmacro-lambda-form [?i ?\\ ?b ?e ?g ?i ?n ?\{ ?\} escape ?i ?e ?q ?a backspace ?u ?a ?t ?i ?o ?n escape ?y ?y ?p ?w ?c ?w ?e ?n ?d escape ?O escape ?\s-s] 0 "%d"))

;; Multiline 

(fset 'latex-frag-mult
   (kmacro-lambda-form [?i ?\\ ?b ?e ?g ?i ?n ?\{ ?e ?q ?u ?a ?t ?i ?o ?n ?\} escape ?y ?y ?p ?l ?w ?w ?c ?w ?s ?p ?l ?i ?t escape ?y ?y ?p ?w ?c ?w ?e ?n ?d escape ?k ?k ?y ?y ?j ?j ?p ?w ?c ?w ?e ?n ?d escape ?k ?O escape] 0 "%d"))

;; Custom Faces

(setq toggle-org-faces-check t)

(defun toggle-org-faces ()
  "Toggle org level headings whether to be normal font size or in increasing font size."
  (interactive)
  (if toggle-org-faces-check
      (progn
        (custom-set-faces
         '(org-level-1 ((t (:inherit outline-1 :height 1.4))))
         '(org-level-2 ((t (:inherit outline-2 :height 1.3))))
         '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
         '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
         '(org-level-5 ((t (:inherit outline-5 :height 1.1)))))
        (setq toggle-org-faces-check nil)
        )
    (progn
        (custom-set-faces
         '(org-level-1 ((t (:inherit outline-1 :height 1.2))))
         '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
         '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
         '(org-level-4 ((t (:inherit outline-4 :height 1.2))))
         '(org-level-5 ((t (:inherit outline-5 :height 1.2)))))
        (setq toggle-org-faces-check t)
        )
  )
)

(pt/leader-keys
  "th" '(toggle-org-faces :which-key "Headings"))

;; Agenda

(setq org-agenda-files '("~/Notes/Todos.org"
                         "~/Notes/Projects.org"
                         "~/Notes/Repeated.org"
                         "~/Notes/Calendar.org"))
(setq org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t)

;; Capture

(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq todos-file "~/Notes/Todos.org"
      projects-file "~/Notes/Projects.org"
      later-file "~/Notes/Later.org"
      repeat-file "~/Notes/Repeated.org"
      calendar-file "~/Notes/Calendar.org")
(setq org-capture-templates
      '(("t" "Todo" entry (file todos-file)
         "* TODO %?\n %i\n")
        ("s" "School" entry (file todos-file)
         "* TODO %? :school:\n")
        ("S" "School Project" entry (file projects-file)
         "* TODO %? [/] :school:\n")
        ("p" "Project" entry (file projects-file)
         "* TODO %? [/]\n#+COOKIE_DATA:todo\n %i\n")
        ("l" "Something for Later" entry (file later-file)
         "** %?\n %i\n")
        ("r" "Repeated Task" entry (file repeated-file)
         "** %?\n %i\n")
        ("i" "Idea" entry (file "~/Notes/Ideas.org")
         "* %?\n %i\n ")
        ("B" "Book" entry (file+headline "~/Notes/Books.org" "Other")
         "** TODO %?\n")
        ("b" "Blog" entry (file create-new-blog-post))
        ("I" "Invoice" entry (file "~/Work/Invoices/Invoices.org")
         "* %?\n#+ENTITY: \n#+ADDRESS: \n#+DUEDATE: \n| Quantity | Description | Unit Price | Total |\n|----------+-------------+------------+-------|")
        ("c" "Calendar Event" entry (file calendar-file)
         "* %?\n")
        ))

;; Refile

(setq org-refile-targets
      '(("~/Notes/Todos.org" :maxlevel . 1)
        ("~/Notes/Projects.org" :maxlevel . 1)
        ("~/Notes/Repeated.org" :maxlevel . 1)
        ("~/Notes/Later.org" :maxlevel . 1)
        ("~/Notes/Ideas.org" :maxlevel . 1)
        ("~/Notes/Books.org" :maxlevel . 1)))

(setq org-refile-allow-creating-parent-nodes 'confirm
      org-refile-use-outline-path 'file
      org-outline-path-complete-in-steps nil)

;; Tags

(setq org-tag-alist '((:startgroup)
                      ("@work" . ?W)
                      ("@home" . ?H)
                      (:endgroup)
                      ("work" . ?w)
                      ("privy" . ?p)
                      ("health" . ?h)
                      ("learn" . ?l)
                      ("school" . ?s)
                      ("dev" . ?d)
                      ("paid" . ?P)
                      ("volunteer" . ?V)
                      ("DAILY" . ?D)
                      ("crypt" . ?c)))

;; Keywords

(setq org-todo-keywords
      '((sequencep "TODO(t)" "NEXT(n)" "ONGOING(o)" "|" "DONE(d/!)")
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
(setq org-agenda-custom-commands
      '(
        ("m" "Main"
         ((agenda ""
                  ((org-agenda-span 'day)
                   (org-agenda-show-all-dates nil)
                   (org-scheduled-past-days 0)
                   (org-agenda-entry-types '(:scheduled :timestamp))))
          (agenda ""
                  ((org-agenda-span 'month)
                   (org-agenda-time-grid nil)
                   (org-agenda-show-all-dates nil)
                   (org-agenda-entry-types '(:deadline))
                   (org-deadline-warning-days 0)
                   (org-agenda-overriding-header "Upcoming Deadlines")))
          (todo "ONGOING"
                ((org-agenda-overriding-header "Ongoing Tasks")))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Tasks")))
          (todo "WAITING"
                ((org-agenda-overriding-header "Waiting On")))
          )
         nil "~/Agenda/all.html")
        ("u" "Upcoming"
         ((agenda ""
                 ((org-agenda-span 'week)
                  (org-agenda-grid nil)
                  (org-agenda-show-all-dates nil)
                  (org-agenda-entry-types '(:scheduled))
                   (org-scheduled-past-days 0)
                  (org-agenda-files '("~/org/Todos.org" "~/org/Projects.org"))
                  (org-agenda-overriding-header "Upcoming Tasks")))
         (agenda ""
                 ((org-agenda-span 'week)
                  (org-agenda-grid nil)
                  (org-agenda-show-all-dates nil)
                  (org-agenda-entry-types '(:deadline))
                  (org-deadline-warning-days 0)
                  (org-agenda-overriding-header "Upcoming Deadlines")))))
        ))

;; Mobile

(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
(setq org-mobile-inbox-for-pull "~/org/flagged.org")
(setq org-mobile-files (list "~/org/Ideas.org"
                             "~/org/Books.org"
                             "~/org/gtd.org"
                             "~/org/Learn.org"
                             "~/org/Shows to watch.org"))

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

;; Source Code Editing

(setq org-src-window-setup 'split-window-below)

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
  (setq org-roam-node-display-template
        (concat "${title:*} "
                (propertize "${tags:10}" 'face 'org-tag)))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n t" . org-roam-tag-add)
         ("C-c n T" . org-roam-tag-remove)
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
    "oj" '(org-journal-new-entry :which-key "journal"))
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

(setq org-latex-listings 'minted)

(add-to-list 'org-latex-packages-alist '("" "minted"))
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

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; Presentations

(use-package org-tree-slide
  :ensure t
  :after org
  :config
  (setq org-tree-slide-slide-in-effect nil))

(pt/leader-keys
  "op" '(org-tree-slide-mode :which-key "Present"))

;; Temporary Buffer

(defun create-tmp-org ()
  "Create a temporary org buffer"
  (interactive)
  (create-file-buffer "tmp.org")
  (persp-add-buffer "tmp.org")
  (switch-to-buffer "tmp.org")
  (org-mode))

(pt/leader-keys
  "oo" '(create-tmp-org :which-key "tmp org"))

;; ExcaliDraw

(use-package org-excalidraw
  :straight (:type git :host github :repo "wdavew/org-excalidraw")
  :config
  (setq org-excalidraw-directory "~/org-images/")
)

;; Writeroom

(use-package writeroom-mode
  :ensure t
  :config
  (setq writeroom-width 130)
  (pt/leader-keys
    "tw" '(writeroom-mode :which-key "Writeroom")))

;; Blog

(defun create-new-blog-post ()
  "Create a new blog post based on passed name and date in blog-dir"
  (interactive)
  (let ((name (read-string "Enter blog title: ")))
    (expand-file-name (format "%s-%s.org"
                              (string-join (string-split name " ") "_")
                              (format-time-string "%d%m%Y"))
                      "~/Blog/")))

;; Programming

(define-key prog-mode-map (kbd "C-c e s") #'eglot)
(define-key prog-mode-map (kbd "C-c e r") #'eglot-reconnect)
(define-key prog-mode-map (kbd "C-c e a") #'eglot-code-actions)
(define-key prog-mode-map (kbd "C-c e p") #'flycheck-previous-error)
(define-key prog-mode-map (kbd "C-c e n") #'flycheck-next-error)

;; Treesitter

(use-package tree-sitter
  :ensure t
  :config
  ;; activate tree-sitter on any buffer containing code for which it has a parser available
  (global-tree-sitter-mode)
  ;; you can easily see the difference tree-sitter-hl-mode makes for python, ts or tsx
  ;; by switching on and off
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter)

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
  :init
  ;; NOTE: Set this to the folder where you keep your Git repos!
  (when (file-directory-p "~/Projects")
    (setq projectile-project-search-path '("~/Projects")))
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-switch-project-action 'neotree-projectile-action)

  ;; (add-to-list 'projectile-globally-ignored-directories "^\\node_modules")
  )

(pt/leader-keys
  "p" '(:ignore p :which-key "projects")
  "pp" '(projectile-switch-project :which-key "switch")
  "pt" '(projectile-test-project :which-key "test")
  "pf" '(projectile-find-file :which-key "find")
  "pr" '(projectile-run-project :whick-key "run")
  "pc" '(projectile-compile-project :which-key "compile"))

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

;; Go

(use-package go-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

;; Nim

(use-package nim-mode
  :ensure t)

;; Lua

(use-package lua-mode
  :ensure t)

;; Haskell

(use-package haskell-mode
  :ensure t)

;; Yuck

(use-package yuck-mode
  :ensure t)

;; Web

(use-package web-mode
	:ensure t
	:config
	(setq web-mode-code-indent-offset tab-width))

;; Emmet

(use-package emmet-mode
  :ensure t
  :bind (:map web-mode-map
              ("C-c w" . emmet-wrap-with-markup))
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

;; Typescript

(use-package typescript-mode
  :ensure t
  :after tree-sitter
  :config
  (define-derived-mode typescriptreact-mode typescript-mode
    "TypeScript TSX")

  ;; use our derived mode for tsx files
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
  ;; by default, typescript-mode is mapped to the treesitter typescript parser
  ;; use our derived mode to map both .tsx AND .ts -> typescriptreact-mode -> treesitter tsx
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx)))

;; TSX

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

;; Svelte

(add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))

;; Astro

(add-to-list 'auto-mode-alist '("\\.astro\\'" . web-mode))

;; Terminal

(use-package vterm
  :ensure t )

(pt/leader-keys
  "oT" '(vterm :which-key "terminal"))

;; Toggle

(use-package vterm-toggle
  :ensure t)

(pt/leader-keys
  "ot" '(vterm-toggle :which-key "terminal"))

;; Auto Closing

(add-hook 'prog-mode-hook 'electric-pair-mode)

;; Language Server Protocol

(use-package eglot
  :ensure t)

;; Snippets

(use-package yasnippet
  :ensure t
  :config (yas-global-mode t)
  :bind (("C-c s s" . yas-insert-snippet)
         ("C-c s n" . yas-new-snippet)
         ("C-c s v" . yas-visit-snippet-file))
  )

;; CSV

(use-package csv-mode
  :ensure t
  :mode ("\\.csv\\'")
  :hook (csv-mode . csv-align-mode)
  )

;; Mermaid-JS

(use-package mermaid-mode
  :ensure t)

(use-package ob-mermaid
  :ensure t
  :after (mermaid-mode)
  :custom
  (setq ob-mermaid-cli-path '~/.local/share/pnpm/mmdc))

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

;; Elfeed

(use-package elfeed
  :ensure t
  :config
  (setq elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
        elfeed-show-entry-switch 'display-buffer)
  )

(pt/leader-keys
  "or" '(elfeed :which-key "elfeed"))

;; Elfeed Protocol

(use-package elfeed-protocol
  :ensure t
  :after elfeed
  :config
  (elfeed-protocol-enable)
  :custom
  (elfeed-use-curl t)
  (elfeed-set-timeout 36000)
  (elfeed-log-level 'debug)
  (elfeed-feeds (list
                 (list "fever+https://parth@rss.kolide.co.nz"
                       :api-url "https://rss.kolide.co.nz/api/fever.php"
                       :password (password-read "Enter password: ")))))

;; Invoice Maker

(defun get-invoice-value (keyword)
  "Get keyword value based on passed value"
  (setq tags (org-collect-keywords keyword))
  (list (cadar tags) (cadadr tags) (cadar (cddr tags)))
  )

(defun create-invoice ()
  "Get key details for generating invoices."
  (interactive)
  (setq values (get-invoice-value '("ENTITY" "ADDRESS" "DUEDATE")))
  (org-table-export "~/Work/Invoices/invoice.csv")
  (async-shell-command (format "invoice -c=/home/parth/Work/Invoices/invoice.csv -e=\"%s\" -a=\"%s\" -d=\"%s\""
                         (car values)
                         (cadr values)
                         (caddr values)))
  )

;; Open URL in reader view

(defun open-firefox-reader (url)
  "Open passed URL in firefox in reader mode"
  (shell-command (format "firefox \"about:reader?url=%s\"" url))
  (message "Link Opened")
  )

(defun open-in-reader (&optional url)
  "Open a given link in reader view"
  (interactive "P")
  (if (stringp url)
      (progn
        (open-firefox-reader url))
    (progn
      (let ((at-point (thing-at-point-url-at-point)))
        (if at-point
            (progn
              (open-firefox-reader (thing-at-point-url-at-point))
              )
          (progn
            (let ((url (read-string "Entery URL: ")))
              (open-firefox-reader url)
              ))))
      )))

(pt/leader-keys
  "oR" '(open-in-reader :which-key "Reader"))

;; Auto Update TODO State

(defun org-auto-update-to-next ()
  "Auto update TODO state to NEXT when marked as done."
  (interactive)
  (org-todo "DONE")
  (org-get-next-sibling)
  (org-todo "NEXT")
  )

(define-key org-mode-map (kbd "C-c t") #'org-auto-update-to-next)

;; Rest Client

(use-package restclient
  :ensure t)
