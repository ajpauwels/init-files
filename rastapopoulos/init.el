
;; TLS settings for raspbian
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; Do not show the startup screen.
(setq inhibit-startup-message t)

;; Disable tool bar, menu bar, scroll bar.
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

;; Show line and column numbers
(global-display-line-numbers-mode)
(setq column-number-mode t)

;; Disable beeping
(setq visible-bell 1)

;; Highlight current line.
(global-hl-line-mode t)

;; Place all backups in one directory
(setq backup-directory-alist '(("" . "~/.emacs.d/backups")))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (add-to-list 'load-path "<path where use-package is installed>")
  (require 'use-package))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("5111a41453244802afd93eed1a434e612a8afbdf19c52384dffab129258bab6e" default))
 '(package-selected-packages
   '(projectile go-mode web-mode ng2-mode tide rst-mode handlebars-mode magit switch-window dap-mode terraform-mode which-key exec-path-from-shell toml-mode company yasnippet flycheck lsp-ui lsp-mode rustic expand-region smartparens ivy-rich counsel ivy warm-night-theme yaml-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Custom theme
(use-package warm-night-theme
  :ensure t
  :load-path "themes"
  :config
  (load-theme 'warm-night))

;; Custom global key bindings
(global-set-key (kbd "C-r") 'replace-regexp)

;; Set tab size to 2 spaces
(setq-default tab-width 4)

;; Prevent use of tab characters by default
(setq-default indent-tabs-mode nil)

;; Improved UX with ivy/counsel/swiper
(use-package ivy
  :ensure t
  :config
  (ivy-mode))

(use-package ivy-rich
  :after ivy
  :ensure t
  :init
  (setq ivy-rich-path-style 'abbrev)
  :config
  (ivy-rich-mode 1))

(use-package counsel
  :after ivy
  :ensure t
  :config
  (counsel-mode))

(use-package swiper
  :after ivy
  :ensure t
  :bind
  (("C-s" . swiper)))

;; Projectile for project management
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

;; Pops up next possible keybindings
(use-package which-key
  :ensure
  :init
  (which-key-mode))

;; Yaml IDE
(use-package yaml-mode
  :init
  :ensure t)

;; Select logical chunks of text with C-=
(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

;; Rust support
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
              ("C-c C-c d" . dap-hydra)
              ("C-c C-c h" . lsp-ui-doc-glance))
  :config
  ;; uncomment for less flashiness
  (setq lsp-eldoc-hook nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm
  (setq-local buffer-save-without-query t))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; for rust-analyzer integration

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  :hook
  (web-mode . lsp)
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-doc-enable nil))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; inline errors

(use-package flycheck
  :ensure
  :config
  (global-flycheck-mode 1))

;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; auto-completion and code snippets

(use-package yasnippet
  :ensure
  :config
  (yas-global-mode 1)
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package company
  :ensure
  :bind
  (:map company-active-map
	("C-n". company-select-next)
	("C-p". company-select-previous)
	("M-<". company-select-first)
	("M->". company-select-last))
  (:map company-mode-map
        ("<tab>". tab-indent-or-complete)
        ("TAB". tab-indent-or-complete))
  :config
  (setq company-tooltip-align-annotations t))

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; for Cargo.toml and other config files

(use-package toml-mode :ensure)


;; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
;; setting up debugging support with dap-mode

(use-package exec-path-from-shell
  :ensure
  :init (exec-path-from-shell-initialize))

(use-package dap-mode
  :ensure
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1)

  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  ;; installs .extension/vscode
  (dap-gdb-lldb-setup)
  (dap-register-debug-template
   "Rust::GDB Run Configuration"
   (list :type "gdb"
	 :request "launch"
	 ;;:env '(("LLDB_DEBUGSERVER_PATH" . "/usr/bin/lldb-server"))
	 ;;:env '(("REDACT_DB_URL" . "mongodb+srv://admin:NlWU4hEEKSed5lJ3F9dzZi03EHTPWyz4@redact.jxrab.mongodb.net/dev?retryWrites=true&w=majority") ("REDACT_DB_NAME" . "redact"))'
	 :name "GDB::Run"
	 :target-module (expand-file-name "~/projects/redact/redact-client/target/debug/redact-client")
	 :gdbpath "rust-gdb"
	 :cwd (expand-file-name "~/projects/redact/redact-client/")
	 :target (expand-file-name "~/projects/redact/redact-client/target/debug/redact-client")
	 ;; uncomment if lldb-mi is not in PATH
	 :lldbmipath "/home/ajp/projects/lldb-mi/build/src/lldb-mi"
	 )))

;; Terraform mode
(use-package terraform-mode
  :ensure)

;; Better window switching
(use-package ace-window
  :ensure
  :config
  (global-set-key (kbd "M-o") 'ace-window))

;; Git workflows
(use-package magit
  :ensure)

;; Handlebars templates
(use-package handlebars-mode
  :ensure)

;; Typescript
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))

;; Angular mode
(use-package ng2-mode
  :ensure)
;; (with-eval-after-load 'typescript-mode (add-hook 'typescript-mode-hook #'lsp))

;; Web files (combined HTML/CSS/JS)
(use-package web-mode
  :ensure
  :config
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2))

(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "vue" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

;; Set super kkey
(setq ns-function-modifier 'super)

;; Golang
(use-package go-mode
  :ensure t
  :config
  (add-hook 'go-mode-hook 'lsp-deferred)
  (add-hook 'before-save-hook 'gofmt-before-save))
