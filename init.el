; My Custom Emacs Config
;;

;; STARTUP
;; -----------------------

(setq inhibit-startup-message t)

;; aesthetics
;; -----------------------

;; System specific config
(setq-default sysTypeSpecific  system-type) ;; get the system-type value

;; Font 
(set-face-attribute 'default nil :font "Fira Code Retina" :height 160)

;; If type is "gnu/linux", override to "wsl/linux" if it's WSL.
(when (and (eq system-type 'gnu/linux)
           (getenv "WSLENV"))
  (set-face-attribute 'default nil :font "Fira Code Retina" :height 130)
    (setq-default sysTypeSpecific "wsl/linux") ;; for later use.
    (setq
    cmdExeBin"/mnt/c/Windows/System32/cmd.exe"
    cmdExeArgs '("/c" "start" "") )
    (setq
    browse-url-generic-program  cmdExeBin
    browse-url-generic-args     cmdExeArgs
    browse-url-browser-function 'browse-url-generic)
    )


;; Interface
(scroll-bar-mode -1)     ; Disable visible scrollbar
(tool-bar-mode -1)       ; Disable toolbar
(menu-bar-mode -1)       ; Disable menu bar


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
  :init (doom-modeline-mode 1)
  :config
  ;; Fix doom-modeline not auto-adjusting height to content
  (advice-add #'fit-window-to-buffer :before (lambda (&rest _) (redisplay t)))
  )

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
   "or" '(org-refile :which-key "refile")
   "op" '(org-pomodoro :which-key "pomodoro")
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
(use-package magit
  :config
  ;; Display magit as full screen buffer
  (setq magit-post-display-buffer-hook
      #'(lambda ()
	  (when (derived-mode-p 'magit-status-mode)
		(delete-other-windows)))))

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
  (visual-line-mode 1))

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

(use-package org-pomodoro
  :after org
  :config
  (setq
   user-alert-configuration (quote ((((:category . "org-pomodoro")) libnotify nil)))
  ))

;; Org-Mode!
(use-package org
  :hook (org-mode . org-mode-setup)
  :config
  (setq org-startup-indented t)
  (setq org-agenda-window-setup 'only-window)
  (setq org-ellipsis "  ▼")
  (setq org-hide-leading-stars nil)
  (org-visual-config)
  (setq org-directory "~/jhu-org/")
  ;; Hide bold, italics, etc markers 
  (setq org-hide-emphasis-markers t) 
  (setq org-agenda-files (directory-files-recursively "~/jhu-org/" "\\.org$"))
  ;; Org-refile set depth of agendas
  (setq org-outline-path-complete-in-steps t)
  (setq org-refile-targets '((nil :maxlevel . 1)
 				 (org-agenda-files :maxlevel . 6)))
  ;; Refile to specific org file with heading path
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)         ; Refile in a single go
  ;; Record a CLOSED tag and date/time when moving to completed state (done or cancelled)
  (setq org-log-done t)
  ;; Log closed TODOs with a note and timestamp
  (setq org-log-done 'note)
  ;; Log todo state changes in drawer of todo
  (setq org-log-into-drawer t)
  ;; Set Org Keywords
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w@/!)" "BLOCK(b@/!)" "|" "DONE(d@!)" "CANCELED(c@)")
          (sequence "RESOURCE(r)" "|")
          (sequence "ACTIVE(a)" "|" "INACTIVE(i)" "COMPLETED(c)")
          ))
  ;; Set tags
  (setq org-tag-alist
        '(
          ("file" . ?f)
          ("next" . ?n)
          ("jira" . ?j)
          ("queue" . ?q)
          ("admin" . ?a)
          ("meeting" . ?m)
          ("datavis" . ?v)
          ("access" . ?c)
          ("consult" . ?t)
          ("dev" . ?d)
          ("icpsr" . ?i)
          ("deid" . ?e)
          ("deia" . ?z)
          ("socsci" . ?s)
          ("outreach" . ?o)
          ("workshop" . ?w)
          ("toread" . ?r)
          ("code" . ?x)
          ))

  (setq org-capture-templates
          '(("t" "Todo" entry (file "~/jhu-org/inbox.org")
          "* TODO %? %^g\n  %U\n")
          ("T" "Todo with Clipboard" entry (file "~/jhu-org/inbox.org")
          "* TODO %? %^g\n  %U\n  %x")
          ("r" "Resource with Clipboard" entry (file "~/jhu-org/inbox.org")
          "* RESOURCE %?\n  %U\n  %x")
          ("c" "Consultation" entry (file "~/jhu-org/consults.org")
           "* ACTIVE %^{Patron Name}: %^{Short Description of Consult} %t %^g\n** Background\n%x\n** Interactions\n%?\n** TODOs")
          ("a"               ; key
          "Article"         ; name
          entry             ; type
          (file "~/jhu-org/notes.org" "Article")  ; target
          "* %^{Title} %(org-set-tags) :article: \n:PROPERTIES:\n:Created: %U\n:Linked: %a\n:END:\n%i\nBrief description:\n%?"  ; template
          :prepend t        ; properties
          :empty-lines 1    ; properties
          :created t        ; properties
          )
          ("p" "Project" entry (file "~/jhu-org/projects.org")
          "* ACTIVE %^{Project Name} [/] %^g \n:PROPERTIES:\n:Description: %^{Brief Description}\n:Created: %U\n:ARCHIVE: %s_archive::* %\\1\n:COOKIE_DATA: todo recursive\n:END:\n%?")
          ("m" "Meeting" entry (file "~/jhu-org/meetings.org")
          "* %^{Meeting Title} %^T\n:PROPERTIES:\n:Description: %^{Brief Description of Meeting}\n** Background\n** Meeting Notes\n%?")
          ("M" "Meeting with Clipboard" entry (file "~/jhu-org/meetings.org")
          "* %^{Meeting Title} %^T\n:PROPERTIES:\n:Description: %^{Brief Description of Meeting}\n** Background\n%x\n** Meeting Notes\n%?")
          ("n" "Note" entry (file "~/Documents/jhu-org/inbox.org")
          "* NOTE %?\n%U" :empty-lines 1)
          ("N" "Note with Clipboard" entry (file "~/jhu-org/todo.org")
          "* NOTE %?\n%U\n   %x" :empty-lines 1)
          ))
  (setq org-agenda-custom-commands
        '(
          ("e" "Exclusively TODOs"
           ((todo "TODO"
                  ((org-agenda-overriding-header "TODO")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "INACTIVE" "ACTIVE" "CANCELED" "RESOURCE")))
                   )))
           )
          ("r" "Monthly review"
           (
            (tags "consult" ((org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo '("TODO" "WAIT")))))
            (tags "next" ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "INACTIVE" "ACTIVE" "CANCELED")))
                          (org-tags-match-list-sublevels 'nil)))
            (agenda "" ((org-agenda-span 'month)
                        (org-agenda-todo-list-sublevels 'indented)
                        (org-agenda-entry-types '(:deadline :scheduled))
            ))
           ))
          ("w" "Weekly review"
                  agenda ""
                  ((org-agenda-start-day "-7d")
                  (org-agenda-span 8)
                  (org-agenda-start-on-weekday 3)
                  (org-agenda-start-with-log-mode '(closed))
                  (org-agenda-archives-mode t)
                  (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo '("DONE" "INACTIVE")))
                  ))
          ("p" "Projects"
           ((todo "TODO|WAIT" (
                         (org-agenda-files '("~/jhu-org/projects.org"))
                         (org-super-agenda-groups
                          '((:auto-outline-path t)))))
            )
           )

            ;; '((:auto-category t))))
  
          ("d" "Daily Tasks"
           (
            (agenda "" ((org-agenda-span 5)
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "INACTIVE" "ACTIVE" "CANCELED" "RESOURCE")))
                       ; (org-agenda-entry-types '(:date :deadline :scheduled))
                        ))
            (alltodo "" ((org-agenda-overriding-header "")
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "INACTIVE" "CANCELED" "RESOURCE")))
                         (org-super-agenda-groups
                          '(
                            (:discard (:todo "RESOURCE"))
                            (:name "To File in LibAnswers"
                                   :tag "file"
                                   :order 10)
                            (:name "Today's TODOs"
                                   :tag "next"
                                   :order 1)
                            (:name "Due Today"
                                   :scheduled today
                                   :deadline today
                                   :todo "today"
                                   :order 4)
                            (:discard (:todo "ACTIVE"))
                            (:name "Overdue"
                                   :deadline past
                                   :order 7)
                            (:name "Important"
                                   :priority "A"
                                   :order 5)
                            (:name "Queue (What to work on next)"
                                   :tag "queue"
                                   :order 2)
                            (:discard (:anything))))))))
  
          ))
 
  ;; Ensure org files saved after a refile
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  )



