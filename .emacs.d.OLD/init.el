;; Automatically download packages from MELPA
(eval-when-compile (defvar use-package-always-ensure))
(setq use-package-always-ensure t)

;; Setup MELPA
(package-initialize)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq custom-file "~/.emacs.d/custom-settings.el")
(load custom-file t)

;; Add line numbers
(global-linum-mode 1)

;; Remove scrollbar
(scroll-bar-mode -1)

;; Don't display a startup message
(setq inhibit-startup-message t)

;; Use actual TAB characters to indent and set tabs to 4 spaces
(setq-default tab-width 4)
(setq-default indent-tabs-mode t)

;; Auto-indent
(electric-indent-mode 1)

;; No beeping all the time
(setq ring-bell-function 'ignore)

;; Disable toolbar
(tool-bar-mode -1)

;; Don't force use of "yes" and "no"
(fset 'yes-or-no-p 'y-or-n-p)

;; Remap C-r to open replace-regexp
(global-set-key (kbd "C-r") 'replace-regexp)

;; Install company for auto-completion
(use-package company
  :ensure t
  :config
  (global-company-mode 1)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t))

;; Auto-completion in js-mode and js2-mode
(use-package company-tern
  :ensure t
  :config
  (global-company-mode 1)
  (add-to-list 'company-backends '(company-tern)))

;; Auto-completion in LSP mode
(use-package company-lsp
  :ensure t
  :config
  (add-to-list 'company-backends '(company-lsp)))

;; Load custom color theme
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox))

;; Bind C-= to expand region for incrementally selecting
;; regions of text
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; Install flycheck package for syntax checking and highlighting
(use-package flycheck
  :ensure t
  :config
  (setq-default flycheck-javascript-eslint-executable "eslint-project-relative")

  ;; Checks for local eslint file before using default eslint
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
				  (or (buffer-file-name) default-directory)
				  "node_modules"))
		   (eslint (and root
						(expand-file-name "node_modules/eslint/bin/eslint.js"
										  root))))
      (when (and eslint (file-executable-p eslint))
		(setq-local flycheck-javascript-eslint-executable eslint))))
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
  (global-flycheck-mode 1)

  ;; Disable jshint since we're using eslint
  (setq-default flycheck-disabled-checkers
				(append flycheck-disabled-checkers
						'(javascript-jshint)))

  ;; Set flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck"))

;; Set tab to actually insert a TAB character
(setq-default indent-tabs-mode t)

;; Enable undo tree, I fucking love this thing
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1))

;; Revert to last save bound to F5
(global-set-key (kbd "<f5>") 'revert-buffer)

;; Open terminal bound to F12
(global-set-key (kbd "<f12>") 'ansi-term)

;; Place backup files in backup directly
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Bind "^" to go up a directory level in directory mod
(eval-when-compile (defvar dired-mode-map)) ; Gets rid of the free variable warning on dired-mode-map
(add-hook 'dired-mode-hook
 (lambda ()
  (define-key dired-mode-map (kbd "^")
    (lambda () (interactive) (find-alternate-file "..")))
  ; was dired-up-directory
  ))

;; Install to autocomplete parentheses
(use-package smartparens
  :ensure t
  :config
  (smartparens-global-mode 1))

;; JAVASCRIPT MODE
;; Install Ivy (auto-completion framework), Counsel (enhances Ivy),
;; and Swiper (replaces isearch)
(use-package counsel
  :ensure t
  :diminish ivy-mode
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-re-builders-alist
   '((t . ivy--regex-ignore-order)))
  :bind
  (("C-s" . swiper)
   ("M-x" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> l" . counsel-find-library)
   ("<f2> i" . counsel-info-lookup-symbol)
   ("<f2> u" . counsel-unicode-char))
  )

;; Install pug mode
(use-package pug-mode
  :ensure t)

;; Install json-mode to show JSON in a nice way
(use-package json-mode
  :ensure t)

;; Run JS scripts directly in emacs
(use-package npm-mode
  :ensure t)

;; Use Tern for JS code analysis and completion
(use-package tern
  :ensure t)

;; js2-mode is best
(use-package js2-mode
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'tern-mode)
  (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (add-hook 'js2-mode-hook (lambda ()
							 (define-key tern-mode-keymap (kbd "M-.") nil)
							 (define-key tern-mode-keymap (kbd "M-,") nil)
							 (define-key js2-mode-map (kbd "M-.") nil)))
  (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode)))
;; Keep js2-mode from saying 'require' is undeclared
(setq js2-include-node-externs t)

;; Remove "M-." from js-mode as well
(define-key js-mode-map (kbd "M-.") nil)
(define-key js-mode-map (kbd "M-?") 'tern-highlight-refs)

;; Refactor js
(use-package js2-refactor
  :ensure t
  :config
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill))

;; For cross-referencing variables across files
(use-package xref-js2
  :ensure t
  :config
  (add-hook 'js2-mode-hook (lambda ()
							 (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))
  
;; Install snippets
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)
  (yas-global-mode 1))

;; Typescript editing
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :config (setq tide-format-options '(:indentSize 4 :tabSize 4 :convertTabsToSpaces nil))
  :hook ((typescript-mode . tide-setup)
		 (typescript-mode . tide-hl-identifier-mode)
		 (before-save . tide-format-before-save)))

;; WEB MODE
;; Install HTML/CSS highlighting
(use-package web-mode
  :ensure t
  :mode ("\\.html\\'" . web-mode)
  :config
  (defun my-web-mode-hook ()
    "Hooks for Web mode. Adjust indents"
    (setq web-mode-markup-indent-offset 4)
    (setq web-mode-attr-indent-offset 4)
    (setq web-mode-css-indent-offset 4)
    (setq web-mode-code-indent-offset 4)
    (setq web-mode-css-indent-offset 4))
  (add-hook 'web-mode-hook  'my-web-mode-hook))

;; GIT SUPPORT
(use-package magit
  :ensure t
  :commands magit-status
  :init)

;; MARKDOWN MODE
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode))

;; YAML MODE
(use-package yaml-mode
  :ensure t)

;; PROJECT MANAGEMENT
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :commands (projectile-find-file projectile-switch-project)
  :init
  :config
  (setq projectile-completion-system 'ivy)
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (projectile-mode))

;; Modeline upgrade
(use-package smart-mode-line
  :ensure t)

(use-package smart-mode-line-powerline-theme
  :ensure t
  :config
  (setq sml/theme 'powerline)
  (sml/setup))
