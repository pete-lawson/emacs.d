;; My Custom Emacs Config
;;

;; STARTUP
;; -----------------------

(setq inhibit-startup-message t)

;; Aesthetics
;; -----------------------

;; System specific config
(setq-default sysTypeSpecific  system-type) ;; get the system-type value
;; Disable titlebar
(set-frame-parameter nil 'undecorated t)

;; Font 
(set-face-attribute 'default nil :font "Fira Code Retina" :height 165)

;; If type is "wsl/linux", then set font smaller (retina scaling) and
;; set cmd.exe path so that links open natively in Windows.
(when (and (eq system-type 'gnu/linux)
           (getenv "WSLENV"))
  (set-face-attribute 'default nil :font "Fira Code Retina" :height 145)
  (setq-default sysTypeSpecific "wsl/linux") ;; for later use.
  (setq
   cmdexebin"/mnt/c/windows/system32/cmd.exe"
   cmdexeargs '("/c" "start" "") )
  (setq
   browse-url-generic-program  cmdexebin
   browse-url-generic-args     cmdexeargs
   browse-url-browser-function 'browse-url-generic)
  )


;; Interface
;; ------------------------
(scroll-bar-mode -1)     ; Disable visible scrollbar
(tool-bar-mode -1)       ; Disable toolbar
(menu-bar-mode -1)       ; Disable menu bar


;; Package Manager
;; ------------------------
(require 'package)

;; set package sources
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("nongnu" . "https://elpa.nongnue.org/nongnu/")
			 ("gnu"  . "https://elpa.gnu.org/packages/")))

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

;; Icon dependency for doom-modeline and neotree
(use-package all-the-icons)
;; Use Doom Theme (with Doom modeline)

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  ;;  (load-theme 'doom-solarized-light t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-solarized-light") ; use "doom-colors" for less minimal icon theme
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
  ;; Set minimum window width 
  (setq doom-modeline-window-width-limit 10)
  ;; Don't show text encoding (to save visual space)
  (setq doom-modeline-buffer-encoding nil)
  ;; Define your custom doom-modeline
  (with-eval-after-load "doom-modeline"
    (doom-modeline-def-modeline 'main
      '(bar workspace-name window-number modals matches buffer-info remote-host parrot misc-info)
      '(selection-info buffer-position word-count objed-state persp-name battery grip irc mu4e gnus github debug lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker)))
  )

;; Use the Modus-Operandi-Tinted high-contrast theme
(use-package modus-themes
  :ensure t
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil)

  ;; Maybe define some palette overrides, such as by using our presets
  (setq modus-themes-common-palette-overrides
        modus-themes-preset-overrides-intense)

  ;; Load the theme of your choice.
  (load-theme 'modus-operandi-tinted :no-confirm))

;; Set Transparency
;;(set-frame-parameter (selected-frame) 'alpha '(98 98))
;;(add-to-list 'default-frame-alist '(alpha 98 98))

;; add line numbers (except in term and org mode)
(column-number-mode)
(global-display-line-numbers-mode t)

;; disable line numbers for org, term, eshell
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
		pdf-view-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Set cursor color
(set-cursor-color "#835083")

;; h1 line mode highlight color
(global-hl-line-mode 1)
(set-face-attribute'hl-line nil :inherit nil :background "#f5eff5")

;; Configure Parentheses 
(setq show-paren-delay 0)
(show-paren-mode 1)
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

(use-package company
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0.3)
  (global-company-mode t))

;; Enable ivy autocompletion
(use-package ivy
  :diminish
  :config
  (ivy-mode 1)
  (define-key ivy-minibuffer-map (kbd "S-SPC") nil)     
  (define-key ivy-minibuffer-map (kbd "M-SPC") 'ivy-restrict-to-matches)
  ;; Get rid of start of line character restriction by removing ^
  (setq ivy-initial-inputs-alist '((counsel-minor . "^+")
				   (counsel-package . "^+")
				   (counsel-org-capture . "^")
				   (counsel-M-x . "")
				   (counsel-describe-symbol . "^")
				   (org-refile . "")
				   (org-agenda-refile . "^")
				   (org-capture-refile . "^")
				   (Man-completion-table . "^")
				   (woman . "^"))))

;; Enable ivy-rich mode
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

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

;; Enable polymode
(use-package poly-markdown
  :ensure t)

;; Enable ess
(use-package ess
  :ensure t
  :init (require 'ess-site)
  :config
  (setq ansi-color-for-comint-mode 'filter))

;; Enable sane undo/redo
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1))

;; Add mode for markdown
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

;; Add mode for Quarto
(use-package quarto-mode
  :mode (("\\.Rmd" . poly-quarto-mode))
  )
;; Replicate surround.vim plugin in Emacs
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; Override the basic Emacs commands
(use-package counsel
  :bind* ; load when pressed 
  (("M-x"     . counsel-M-x)
   ("C-s"     . swiper)
   ("C-c l"   . org-store-link)
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

    ;; Find
    "f" '(:ignore t :which-key "file")
    "ff" '(counsel-find-file :which-key "find file")
    "fr" '(counsel-recentf :which-key "recent files")
    "fb" '(swiper :which-key "search in buffer")
    "fp" '(counsel-projectile-rg :which-key "search in project")

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
    "wk" '(windmove-up :which-key"select-up")
    "wd" '(delete-window :which-key "delete")
    "wf" '(toggle-frame-fullscreen :which-key "fullscreen")

    ;; Org-Mode
    "o" '(:ignore t :which-key "org")
    "oa" '(org-agenda :which-key "agenda")
    "oi" '(:ignore t :which-key "insert")
    "or" '(org-refile :which-key "refile")
    "os" '(org-sort :which-key "sort")
    "ot" '(org-change-tag-in-region :which-key "bulk tag")
    "op" '(org-pomodoro :which-key "pomodoro")
    "om" '(hydra-org-move/body :which-key "move subtree")
    "oih" '(org-insert-heading-respect-content :which-key "insert heading")
    "ois" '(org-insert-subheading :which-key "insert subheading")
    "oit" '(org-insert-todo-heading-respect-content :which-key "insert todo")
    "oiT" '(org-insert-todo-subheading :which-key "insert todo subheading")
    "oib" '(org-insert-heading-after-current :which-key "insert heading after current")
    "oc" '(org-capture :which-key "capture")
    "og" '(counsel-org-goto :which-key "goto")
    "oG" '(counsel-org-goto-all :which-key "goto all")
    "oT" '(org-tags-view :which-key "org-headlines by tag")
    "o/" '(org-search-view :which-key "org by keyword")
    "of" '(org-narrow-to-subtree :which-key "focus on current subtree")
    "oF" '(widen :which-key "remove focus")
    "ob" '(org-sidebar-toggle :which-key "toggle sidebar")


    ;; Org-capture
    "c" '(org-capture :which-key "org-capture")

    ;; Clipboard history
    "k" '(counsel-yank-pop :which-key "kill ring")

    ;; org-roam
    "r" '(:ignore t :which-key "org-roam")
    "rb" '(org-roam-buffer-toggle :which-key "toggle roam buffer")
    "rf" '(org-roam-node-find :which-key "find roam node")
    "ri" '(org-roam-node-insert :which-key "insert roam node")
    "rs" '(org-roam-db-sync :which-key "sync roam database")
    "rg" '(org-roam-graph :which-key "roam graph")
    "ru" '(org-roam-ui-open :which-key "open org-roam ui")

    ;; Rapid frame switching
    "j" '(other-frame :which-key "switch frame")

    ;; Avy
    "a" '(:ignore t :which-key "avy")
    "ac" '(avy-goto-char :which-key "go to char")
    "ax" '(avy-goto-char-2 :which-key "go to char 2")
    ))


;; Hydra for active reactive commands (increase/decrease font for example)
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(defhydra hydra-org-move (:timeout 4)
  "move org subtree"
  ("j" org-move-subtree-down "down")
  ("k" org-move-subtree-up "up")
  ("h" org-promote-subtree "promote")
  ("l" org-demote-subtree "demote")
  )


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


;; Use auctex for LaTeX
(use-package auctex
  :ensure t
  :defer t
  :config
  (setq TeX-PDF-mode t))

(use-package pdf-tools
  :ensure t
  ;;:mode ("\.pdf\'" . pdf-view-mode)
  :config
  ;; initialize
  (pdf-tools-install))

;; to use pdfview with auctex
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
      TeX-source-correlate-start-server t) ;; not sure if last line is neccessary

;; to have the buffer refresh after compilation
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)

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

;; Set inline imagess to width of 400 pixels
(setq org-startup-with-inline-images t)
(setq org-image-actual-width 500)

;;Configure org-mode visual mode 
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
  ;;(setq org-superstar-leading-bullet ?\s)
  (with-eval-after-load 'org-superstar
    (set-face-attribute 'org-superstar-header-bullet nil :height .8)
  ))

(use-package org-super-agenda
  :after org
  :hook (org-mode . org-super-agenda-mode))

(use-package org-ql)

(use-package org-pomodoro
  :after org
  :config
  (setq
   user-alert-configuration (quote ((((:category . "org-pomodoro")) libnotify nil)))
   )
  (setq org-pomodoro-format "%s")
  )
  (setq visual-fill-column-center-text t
	visual-fill-column-width 100)

;; Org-Mode!
(use-package org
  :hook (org-mode . org-mode-setup)
  :config

  ;; Make org beautiful. Create a readable org environment by
  ;; configuring variable width font with the ETBembo font (inspired
  ;; by Tufte)
  ;; 
  ;; -------------------
  ;; Hide bold, italics, etc markers 
  (setq org-hide-emphasis-markers t)

  ;; Enable variable pitch mode in org-mode exclusively
  (add-hook 'org-mode-hook 'variable-pitch-mode)

  ;; Enable line wrapping: instead of setting an explicit width, text
  ;; wraps to the window width. Will need to remember to press M-q to
  ;; adjust the current paragraph.
  (add-hook 'org-mode-hook 'visual-line-mode)

  ;; Hide leading stars for clean nested bullets
  (setq org-hide-leading-stars t)

  ;; Set fixed pitch and variable pitch fonts
  (custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "ETBembo" :height 1.0))))
   '(fixed-pitch ((t ( :family "Fira Code Retina" :height 135)))))

  ;; We don't want everything to be variable pitch. Fix it with
  ;; custom faces.
  (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 1.3))))
   '(org-level-1 ((t (:inherit variable-pitch :weight semi-bold :height 1.3))) t)
   '(org-level-2 ((t (:inherit variable-pitch :height 1.2))) t)
   '(org-level-3 ((t (:inherit variable-pitch :height 1.1))) t)
   '(org-level-4 ((t (:inherit variable-pitch))) t)
   '(org-level-5 ((t (:inherit variable-pitch))) t)
   '(org-level-6 ((t (:inherit variable-pitch))) t)
   '(org-level-7 ((t (:inherit variable-pitch))) t)
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

  ;; Scale the size of LaTeX previews
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

  ;; It's done. Org is beautiful.
  ;; -------------------------------

  (setq org-startup-indented t)
  (setq org-enforce-todo-dependencies t)
  (setq org-agenda-dim-blocked-tasks 'nil)
  (setq org-agenda-window-setup 'only-window)
					;  (setq org-agenda-tags-column -100)
					;  (setq org-auto-align-tags t)
					;  (setq org-tags-column -100)
					;(org-visual-config)
  (setq org-directory "~/jhu-org/")


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

  ;; configure org-goto to jump to headline
  ;; (setq org-goto-interface 'outline-path-completion
  ;; 	org-goto-max-level 10)
  (setq org-outline-path-complete-in-steps nil)

  ;; Truncate long org-clock tasks so they fit in mode-line
  (setq org-clock-heading-function
	(lambda ()
	  (let ((str (nth 4 (org-heading-components))))
	    (if (> (length str) 20)
		(substring str 0 20)))))

  ;; Set Org Keywords
  (setq org-todo-keywords
	'((sequence "TODO(t)" "IN-PROGRESS(p)" "WAIT(w)" "|" "DONE(d@!)" "CANCELED(c@)")
          (sequence "RESOURCE(r)" "|")
	  (sequence "MEETING(m)" "SEMINAR(s)" "WORKSHOP(W)" "|")
          (sequence "ACTIVE(a)" "DEFERRED(i)" "|" "COMPLETED(c)")
          (sequence "TRANS-ZOOM(z)" "TRANS-EMAIL(e)" "|" "TRANS-FILED(f)")
          ))
  ;; Color those keywords
  (setq org-todo-keyword-faces
	'(
	  ("TODO" . (:foreground "#8f0075" :weight bold :inherit fixed-pitch :height 1.1))
          ("IN-PROGRESS" . (:foreground "#354fcf" :weight bold :inherit fixed-pitch :height 1.1))
          ("DONE" . (:foregrond "#316500" :weight bold :inherit fixed-pitch :height 1.1))
          ("WAIT" . (:foreground "goldenrod1" :weight bold :inherit fixed-pitch :height 1.1))
          ("BLOCK" . (:foreground "#a60000" :weight bold :inherit fixed-pitch :height 1.1))
          ("RESOURCE" . (:foreground "DarkMagenta" :weight bold :inherit fixed-pitch :height 1.1))
          ("CANCELED" . (:foreground "blue" :weight bold :inherit fixed-pitch :height 1.1))
          ("MEETING" . (:foreground "RoyalBlue4" :weight bold :inherit fixed-pitch :height 1.1))
          ("SEMINAR" . (:foreground "LightSteelBlue1" :background "purple2" :weight bold :inherit fixed-pitch :height 1.1))
          ("WORKSHOP" . (:foreground "purple4" :weight bold :inherit fixed-pitch :height 1.1))
          ("TRANS-ZOOM" . (:foreground "MediumPurple1" :weight bold :inherit fixed-pitch :height 1.1))
          ("TRANS-EMAIL" . (:foreground "MediumPurple2" :weight bold :inherit fixed-pitch :height 1.1))
	  ))

  ;; Set tags
  (setq org-tag-alist
	'(
          ("file" . ?f)
          ("next" . ?n)
          ("blocking" . ?b)
          ("jira" . ?j)
          ("admin" . ?a)
          ("meeting" . ?m)
          ("datavis" . ?v)
          ("access" . ?c)
          ("consult" . ?t)
          ("dev" . ?d)
	  ("active" . ?e)
          ("icpsr" . ?i)
          ("outreach" . ?o)
          ("workshop" . ?w)
          ("toread" . ?r)
          ))

  ;; Set tag faces
  (setq org-tag-faces
	'(("blocking" . (:foreground "white" :background "red3" :weight bold :inherit fixed-pitch :height 1.1))))

  (setq org-capture-templates
	'(
	  ("t" "Task")
	  ("tt" "Todo" entry (file "~/jhu-org/inbox.org")
           "* TODO %? %^G\n  %U\n")
          ("tT" "Todo with Clipboard" entry (file "~/jhu-org/inbox.org")
           "* TODO %? %^G\n  %U\n  %x")
	  ("ts" "Scheduled Todo" entry (file "~/jhu-org/inbox.org")
           "* TODO %? %^G\n :SCHEDULED %^t\n")
          ("tS" "Scheduled Todo with Clipboard" entry (file "~/jhu-org/inbox.org")
           "* TODO %? %^G\n :SCHEDULED %^t \n  %x")
	  ("c" "Consultations")
          ("cn" "New Consult" entry (file "~/jhu-org/consults.org")
           "* %^{Short Description of Consult} | %^{Patron Name} | %^{School} %^G\n** Interactions\n** TODOs")
          ("cz" "Zoom Consult" entry (file "~/jhu-org/consults.org")
           "* TRANS-ZOOM Zoom Consult w/ %^{Patron Name}: %^{Short Description} %^t :file:%^g\n
            %?")
          ("cZ" "Zoom Consult w/ Clipboard" entry (file "~/jhu-org/consults.org")
           "* TRANS-ZOOM Zoom Consult w/ %^{Patron Name}: %^{Short Description} %^t :zoom:file:%^g\n
            %x%?")
          ("ce" "Email Consult" entry (file "~/jhu-org/consults.org")
           "* TRANS-EMAIL Email Consult w/ %^{Patron Name}: %^{Short Description} %t :file:%^g\n
            %?")
          ("cE" "Email Consult w/ Clipboard" entry (file "~/jhu-org/consults.org")
           "* TRANS-EMAIL Email Consult w/ %^{Patron Name}: %^{Short Description} %t :file:%^g\n
            %x%?")
          ("r" "Resource with Clipboard" entry (file "~/jhu-org/inbox.org")
           "* RESOURCE %?\n  %U\n  %x")
	  ("m" "Meetings")
          ("mm" "Meeting" entry (file "~/jhu-org/meetings.org")
           "* MEETING %^{Meeting Title} %^T\n:PROPERTIES:\n:Description: %^{Brief Description of Meeting}\n** Background\n** Meeting Notes\n%?")
          ("mM" "Meeting with Clipboard" entry (file "~/jhu-org/meetings.org")
           "* MEETING %^{Meeting Title} %^T\n:PROPERTIES:\n:Description: %^{Brief Description of Meeting}\n** Background\n%x\n** Meeting Notes\n%?")
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
	  ("n" "Note")
          ("nn" "Note" entry (file "~/Documents/jhu-org/inbox.org")
           "* NOTE %?\n%U" :empty-lines 1)
          ("nN" "Note with Clipboard" entry (file "~/jhu-org/todo.org")
           "* NOTE %?\n%U\n   %x" :empty-lines 1)
          ))

  (setq org-agenda-custom-commands
	'(
          ("e" "Exclusively TODOs"
           ((todo "TODO"
                  ((org-agenda-overriding-header "TODO")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'regexp ":email:" 'todo '("DONE" "INACTIVE" "ACTIVE" "CANCELED" "RESOURCE")))
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
			       (org-agenda-skip-function '(org-agenda-skip-entry-if 'notregexp "\\[#A\\]"))
			       (org-super-agenda-groups
				'((:auto-outline-path t)))))
            )
           )
          ("n" "Next"
           ((todo "TODO|WAIT" (
			       (org-agenda-files '("~/jhu-org/todo.org"))
			       (org-super-agenda-groups
				'((:auto-outline-path t)))))
            )
           )
          ("c" "Consults"
           ((todo "TODO|WAIT" (
                               (org-agenda-files '("~/jhu-org/consults.org"))
                               (org-super-agenda-groups
				'((:auto-outline-path t)))))
            )
           )
          ;; '((:auto-category t))))
	  
          ("d" "Daily Tasks"
           (
            (agenda "" ((org-agenda-span 6)
			(org-agenda-files '("~/jhu-org/meetings.org" "~/jhu-org/consults.org" "~/jhu-org/calendar.org" "~/jhu-org/todo.org" "~/jhu-org/projects.org"))
			(org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("TRANS-EMAIL" "DONE" "INACTIVE" "ACTIVE" "CANCELED" "RESOURCE")))
					; (org-agenda-entry-types '(:date :deadline :scheduled))
			))
            (alltodo "" ((org-agenda-overriding-header "")
			 (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("INACTIVE" "CANCELED")))
			 (org-super-agenda-groups
                          '(
                            (:name "Today's TODOs that are blocking someone"
				   :and (:tag "next" :tag "blocking")
                                   :order 1)
                            (:name "Today's remaining TODOs"
                                   :tag "next"
                                   :order 2)
                            (:name "Due Today"
                                   :scheduled today
                                   :deadline today
                                   :todo "today"
                                   :order 4)
                            (:name "To File in LibAnswers"
				   :and (
					 :todo ("TRANS-EMAIL" "TRANS-ZOOM")
					 :tag "file")
				   :order 6)
                            (:discard (:todo "ACTIVE"))
                            (:name "Overdue"
                                   :deadline past
                                   :order 7)
                            (:discard (:anything))))))))

          ))

  ;; Ensure org files saved after a refile
  (advice-add 'org-refile :after 'org-save-all-org-buffers))
;; Add visual padding to left of org-mode
(use-package visual-fill-column
  :hook (org-mode . org-visual-config))

(use-package org-super-agenda
  :after org
  :hook (org-mode . org-super-agenda-mode))

(use-package org-pomodoro
  :after org
  :config
  (setq
   user-alert-configuration (quote ((((:category . "org-pomodoro")) libnotify nil)))
   )
  (setq org-pomodoro-format "%s")
  )

(use-package org-sidebar
  :after org
  )
;; org-roam
(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/org-work/mind-palace")
  (setq org-roam-db-autosync-mode t) 
  )



;; Code
;; -----------------------

(use-package aggressive-indent
  :hook (emacs-lisp-mode . aggressive-indent-mode))

;; Navigation
(use-package avy)

