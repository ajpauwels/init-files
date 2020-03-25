;;; init.el --- Alexandre Pauwels' custom emacs config

;;; Commentary:
;;  - Sets up Emacs v26.3

;;; Code:

;; Setup MELPA
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(package-initialize)

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Setup use-package
(eval-when-compile
  (require 'use-package))

;; Avoid having Custom config at end of init.el
(setq custom-file "~/.emacs.d/custom-settings.el")
(load custom-file t)

;; Install theme first
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;; Add line numbers
(global-linum-mode t)

;; Remove scrollbar
(scroll-bar-mode -1)

;; Disable toolbar
(tool-bar-mode -1)

;; Disable menubar
(menu-bar-mode -1)

;; Don't display a startup screen
(setq inhibit-startup-screen t)

;; No beeping all the time
(setq ring-bell-function 'ignore)

;; Disable cursor blinking
(blink-cursor-mode 0)

;; Force newline at end of files
(setq require-final-newline t)

;; Don't force use of "yes" and "no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Remap C-r to open replace-regexp
(global-set-key (kbd "C-r") 'replace-regexp)

;; Revert to last save bound to F5
(global-set-key (kbd "<f5>") 'revert-buffer)

;; Place backup files in backup directly
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Bind C-= to expand region for incrementally selecting
;; regions of text
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; Auto-close brackets and parens
(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode 1))

;; Quick window switching
(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window))

;; Project manager
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-project-search-path '("~/projects/")))

;; Automatically indent everything
(use-package aggressive-indent
  :ensure t
  :config
  (global-aggressive-indent-mode 1))

;; Syntax checker
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Treemacs for helpful displaying of filesystem
(use-package treemacs
  :ensure t
  :defer t
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

;; Better treemacs integration with projectile
(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

;; Add support for Language Server Protocol
(use-package lsp-mode
  :ensure t
  :hook
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp)

;; Set prefix for lsp-command-keymap
(setq lsp-keymap-prefix "s-l")

;; Provides visual feedback of LSP output
(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :commands lsp-ui-mode)

;; Auto-completion
(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  ;; Recommend config by LSP documentation
  (setq company-minimum-prefix-length 1
	company-idle-delay 0.0))

;; Auto-completion using LSP output
(use-package company-lsp
  :ensure t
  :after lsp-mode company
  :commands company-lsp)

;; if you are ivy user
(use-package lsp-ivy
  :ensure t
  :after lsp-mode ivy
  :commands lsp-ivy-workspace-symbol)

;; Integrate better with treemacs
(use-package lsp-treemacs
  :ensure t
  :after lsp-mode treemacs
  :commands lsp-treemacs-errors-list)

;; optional if you want which-key integration
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Enable undo tree
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1))

;; Setup Ivy
(use-package ivy
  :ensure t
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume))

;; Setup Counsel
(use-package counsel
  :ensure t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c a") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

;; Setup Swiper
(use-package swiper
  :ensure t
  :config
  (global-set-key "\C-s" 'swiper))

;; Support Markdown files
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode))

;; Support YAML files
(use-package yaml-mode
  :ensure t)

;; Support Rust projects
(use-package rustic
  :ensure t
  :config
  (setq rustic-format-on-save t)
  (setq rustic-compile-command "cargo build")
  (setq counsel-compile-history '("cargo build")))

;;; init.el ends here
