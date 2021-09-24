;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq doom-font (font-spec
                 :family "Fira Code"
                 :size 12
                 :weight 'normal))

(require 'doom-themes)
(setq doom-theme 'doom-city-lights)
(setq display-line-numbers-type 'relative)
(setq-default tab-width 2)

;; Transparent adjustment
;; (set-frame-parameter (selected-frame)'alpha '(95 . 90))
;; (add-to-list 'default-frame-alist'(alpha . (90 . 90)))

; Org Directory
(setq org-directory "~/org/")
(setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))

;; Org config
(setq org-hide-emphasis-markers t)
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


;; Org Roam
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/RoamNotes")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

;; Custom python settings
(setq python-indent-offset 4)
(defun python-custom-settings ()
  (setq tab-width 4))

(add-hook 'python-mode-hook 'python-custom-settings)

;; Latex
(setq org-latex-create-formula-image-program 'dvipng)

;; Org headings
(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.1))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
)

;; Org exports
(map! :leader
      (:prefix-map ("d". "org")
       :desc "Export org to ODT" "w" #'org-odt-export-to-odt
       :desc "Export ort to HTML" "h" #'org-html-export-to-html
       :desc "Open TODO" "a" #'org-agenda
       :desc "Toggle LaTeX" "l" #'org-preview-latex-fragment
       ))
