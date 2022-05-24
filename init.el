; My Custom Emacs Config
;;

;; STARTUP
;; -----------------------

(setq inhibit-startup-message t)

;; aesthetics
;; -----------------------

;; Font 
(set-face-attribute 'default nil :font "Fira Code Retina" :height 140)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "Fira Code Retina" :height 160)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "ETBembo" :height 180 :weight 'regular :width 'ultra-expanded)

;; Interface
(scroll-bar-mode -1)     ; Disable visible scrollbar
(tool-bar-mode -1)       ; Disable toolbar
(menu-bar-mode -1)       ; Disable menu bar
 
;; Theme

;; Use Doom Theme (with Doom modeline)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Doom Modeline

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Set Transparency
(set-frame-parameter (selected-frame) 'alpha '(98 98))
(add-to-list 'default-frame-alist '(alpha 98 98))

;; add line numbers (except in term and org mode)
(column-number-mode)
(global-display-line-numbers-mode t)

;; disable line numbers for org, term, eshell
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; add color matched brackets, braces, parentheses
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; use which-key to provide keybinding context menu
(use-package which-key
  :init (setq which-key-idle-delay .001)
  (which-key-mode)
  :diminish which-key-mode)

;; Package Manager
;; ------------------------
(require 'package)

;; set package sources
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org"   . "https://orgmode.org/elpa/")
			 ("elpa"  . "https://elpa.gnu.org/packages/")))

;; Ensure package archives are downloaded
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install packages if they are not already installed
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Quit prompts with ESC
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


;; Enable ivy autocompletion
(use-package ivy
  :diminish
  :config
  (ivy-mode 1))

;; Enable ivy-rich mode
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; Override the basic Emacs commands
(use-package counsel
  :bind* ; load when pressed 
  (("M-x"     . counsel-M-x)
   ("C-s"     . swiper)
   ("C-x b"   . counsel-ibuffer)
   ("C-x c-f" . counsel-find-file)
   ("C-x c-r" . counsel-recentf)  ; search for recently edited
   ("C-c g"   . counsel-git)      ; search for files in git repo
   ("C-c j"   . counsel-git-grep) ; search for regexp in git repo
   ("C-c /"   . counsel-ag)       ; use ag for regexp
   ("C-x l"   . counsel-locate)
   ("C-x t"   . counsel-load-theme)
   ("C-x c-f" . counsel-find-file)
   ("<f1> f"  . counsel-describe-function)
   ("<f1> v"  . counsel-describe-variable)
   ("<f1> l"  . counsel-find-library)
   ("<f2> i"  . counsel-info-lookup-symbol)
   ("<f2> u"  . counsel-unicode-char)
   ("C-c C-r" . ivy-resume)))     ; resume last ivy-based completion

;; Use General for keybindings
(use-package general
  :config
  (general-create-definer leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (leader-keys
    ;; Theme
   "t" '(:ignore t :which-key "theming")
   "tt" '(counsel-load-theme :which-key "choose-theme")
   "ts" '(hydra-text-scale/body :which-key "scale text")

   ;; Buffer
   "b" '(:ignore t :which-key "buffer")
   "be" '(eval-buffer :which-key "eval buffer")
   "bb" '(switch-to-buffer :which-key "switch buffer")
   "bj" '(switch-to-next-buffer :which-key "next buffer")
   "bk" '(switch-to-prev-buffer :which-key "previous buffer")
   "be" '(eval-buffer :which-key "eval buffer")
   "bd" '(kill-buffer :which-key "delete buffer")

   ;; Search  
   "f" '(:ignore t :which-key "find")
   "fb" '(swiper :which-key "find in buffer")
   "ff" '(find-file :which-key "find file")
   "fr" '(counsel-recentf :which-key "recent files")

   ;; Project
   "p" '(:ignore t :which-key "project")
   "pp" '(counsel-projectile-switch-project :which-key "switch project")
   "pf" '(counsel-projectile-rg :which-key "find file")

   ;; Git
   "g" '(:ignore t :which-key "git")
   "gg" '(magit-status :which-key "status")

   ;; Window 
   "w" '(:ignore t :which-key "window")
   "ws" '(split-window-below :which-key "horizontal-split")
   "wv" '(split-window-right :which-key "vertical-split")
   "wl" '(windmove-right :which-key "select-right")
   "wh" '(windmove-left :which-key "select-left")
   "wj" '(windmove-down :which-key "select-down")
   "wk" '(windmove-up :which-key "select-up")
   "wd" '(delete-window :which-key "delete")

   ;; Org-Mode
   "o" '(:ignore t :which-key "org")
   "oa" '(org-agenda :which-key "agenda")
   "oi" '(:ignore t :which-key "insert")
   "om" '(hydra-org-move/body :which-key "move subtree")
   "oih" '(org-insert-heading :which-key "insert-heading")
   "ois" '(org-insert-subheading :which-key "insert-subheading")
   "oc" '(org-capture :which-key "capture")
  ))


;; Hydra for active reactive commands (increase/decrease font for example)
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-increase "out")
  ("f" nil "finished" :exit t))

(defhydra hydra-org-move (:timeout 4)
  "move org subtree"
  ("j" org-move-subtree-down "down")
  ("k" org-move-subtree-up "up")
  ("h" org-promote-subtree "promote")
  ("l" org-demote-subtree "demote")
  )

;; Be Evil!
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

;; Add missing evil keybindings
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

;; Improved help menu
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key]      . helpful-key))

;; Use projectile for managing projects
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/dev")
    (setq projectile-project-search-path '("~/dev")))
  ;; Set the first thing projectile does when switching to a new project.
  (setq projectile-switch-project-action #'projectile-dired))

;; Use ivy integration for extra actions with alt-o
(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; Move custom set variables to separate directory
(setq custom-file (concat user-emacs-directory "/custom.el"))

;; Use magit for VC
(use-package magit)

;; Recent File Finder
(use-package recentf
      :bind ("C-x C-r" . recentf-open-files)
      :config
      (recentf-mode 1)
      (setq recentf-max-menu-items 15
            recentf-max-saved-items 100)
      :hook (after-init . recentf-mode))

;; Org Configuration custom functions

;; General org-mode config
(defun org-mode-setup ()
  (visual-line-mode 1)
  (variable-pitch-mode 1))

;; Configure org-mode font
(defun org-font-setup ()

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.3)
                  (org-level-2 . 1.2)
                  (org-level-3 . 1.1)
                  (org-level-4 . 1.1)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "ETBembo" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

;; Configure org-mode visual mode 
(defun org-visual-config ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

;; Add visual padding to left of org-mode
(use-package visual-fill-column
  :hook (org-mode . org-visual-config))

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-leading-bullet ?\s))

(use-package org-super-agenda
  :after org
  :hook (org-mode . org-super-agenda-mode))

;; Org-Mode!
(use-package org
  :hook (org-mode . org-mode-setup)
  :config
  (setq org-ellipsis "  ▼")
  (setq org-hide-leading-stars nil)
  (org-font-setup)
  (org-visual-config)

  ;; Ensure org files saved after a refile
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  )



