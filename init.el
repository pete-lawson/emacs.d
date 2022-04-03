; My Custom Emacs Config
;;

;; STARTUP
;; -----------------------

(setq inhibit-startup-message t)

;; aesthetics
;; -----------------------

;; Font 
(set-face-attribute 'default nil :font "Source Code Pro" :height 140)

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
(use-package general)

;; Be Evil!
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybindings nil)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

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

;; Move custom set variables to separate directory
(setq custom-file (concat user-emacs-directory "/custom.el"))
