;;; init.el -- Emacs configuration:
;;============================
;; Code:
(prefer-coding-system 'utf-8)
(tool-bar-mode 0)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)
;;=============================
;; Internal:
(setq font-lock-maximum-decoration t)

(set-face-attribute 'default nil :font "Source Code Pro for Powerline 10")
(set-frame-font "Source Code Pro for Powerline 10" nil t)

(setq column-number-mode t)

(setq default-buffer-file-coding-system 'utf-8)
;;add melpa repo.
;; ================
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/") t) ; Org-mode's repository
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
;; Keys binding
(setq truncate-lines t)
;; Keys for windows
(windmove-default-keybindings)
(global-set-key (kbd "C-c b") 'windmove-left)
(global-set-key (kbd "C-c f") 'windmove-right)
(global-set-key (kbd "C-c p") 'windmove-up)
(global-set-key (kbd "C-c n") 'windmove-down)
(global-set-key (kbd "C-c h") 'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)
(global-set-key (kbd "C-c k") 'windmove-up)
(global-set-key (kbd "C-c j") 'windmove-down)
;; ====

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

;;; Company mode:
(setq company-backends '(company-elisp
			  company-ropemacs
			  company-gtags
			  company-dabbrev-code
			  company-keywords
			  company-files
			  company-dabbrev))

(add-hook 'after-init-hook 'global-company-mode)
(put 'upcase-region 'disabled nil)

;;================
;; Flycheck mode:
(add-hook 'after-init-hook #'global-flycheck-mode)
(require 'flycheck-color-mode-line)
(eval-after-load "flycheck"
		 '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

;;=============================
;; Linum
;;=============================
;;(require 'linum)
;;(add-hook 'after-init-hook #'linum-mode)
;;=============================

;; Nyan mode.
;;=============================
(add-hook 'after-init-hook 'nyan-mode)
(add-hook 'nyan-mode-hook 'nyan-start-animation)

;;==========
;; js2-mode
;;==========
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(require 'js2-refactor)
(js2r-add-keybindings-with-prefix "C-c C-m")
;; eg. extract function with `C-c C-m ef`.
;; scss
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)
;; CoffeeScript
(require 'coffee-mode)
;; rainbow-mode for CSS
(require 'rainbow-mode)
;; rainbow-delimiters
(require 'rainbow-delimiters)
;; (global-rainbow-delimiters-mode)
;;; Magit
(require 'magit)
(global-set-key (kbd "C-c m g") 'magit-status)
;; Color theme
;;(require 'sublime-themes)
;;(load-theme 'fogus t)
(require 'solarized-theme)
(load-theme 'solarized-light t)
;;(require 'color-theme)
;;(color-theme-initialize)
;;(add-to-list 'custom-theme-load-path "/home/hel/.emacs.d/themes/base16-emacs")
;;(load-theme 'brin t)
;; smartparens
(require 'smartparens-config)
(require 'smartparens-ruby)
(smartparens-global-mode)
(show-smartparens-global-mode t)
(sp-with-modes '(rhtml-mode)
	       (sp-local-pair "<" ">")
	       (sp-local-pair "<%" "%>"))

;; ruby mode
(add-hook 'ruby-mode-hook 'robe-mode)
(add-hook 'ruby-mode-hook 'yard-mode)
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))


(require 'cl) ; If you don't have it already

(defun* get-closest-gemfile-root (&optional (file "Gemfile"))
	"Determine the pathname of the first instance of FILE starting from the current directory towards root.
	This may not do the correct thing in presence of links. If it does not find FILE, then it shall return the name
	of FILE in the current directory, suitable for creation"
	(let ((root (expand-file-name "/"))) ; the win32 builds should translate this correctly
	  (loop
	    for d = default-directory then (expand-file-name ".." d)
	    if (file-exists-p (expand-file-name file d))
	    return d
	    if (equal d root) return nil)))

;; prcjectile rails

(require 'grizzl)
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'grizzl)
;; Press Command-p for fuzzy find in project
(global-set-key (kbd "s-p") 'projectile-find-file)
;; Press Command-b for fuzzy switch buffer
(global-set-key (kbd "s-b") 'projectile-switch-to-buffer)

(add-hook 'projectile-mode-hook 'projectile-rails-on)

;; webmode
(require 'web-mode)

;; yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; rbenv
(setq rbenv-installation-dir "~/.rbenv/bin/rbenv")
(require 'rbenv)
;; RBENV
(autoload 'rbenv "rbenv")
(autoload 'global-rbenv-mode "rbenv")

(defalias 'inf-ruby-keys 'inf-ruby-setup-keybindings)

;; For electric goodness!
(require 'ruby-electric)
(add-hook 'ruby-mode-hook (lambda ()
			    (unless (derived-mode-p 'prog-mode)
			      (run-hooks 'prog-mode-hook))
			    (require 'ruby-electric)
			    (ruby-electric-mode t)))

(eval-after-load 'ruby-mode
		 '(progn
		    ;; work around possible elpa bug
		    (ignore-errors (require 'ruby-compilation))
		    (setq ruby-use-encoding-map nil)
		    (autoload 'inf-ruby "inf-ruby")
		    (add-hook 'ruby-mode-hook 'inf-ruby-keys)
		    (autoload 'inf-ruby-switch-setup "inf-ruby")
		    (add-hook 'ruby-mode-hook 'flymake-ruby-load)
		    (require 'ruby-tools)
		    (ruby-tools-mode +1)
		    (require 'ruby-block)
		    (ruby-block-mode t)
		    (add-hook 'ruby-mode-hook 'ri-bind-key)
		    (let ((m ruby-mode-map))
		      (define-key m (kbd "RET") 'reindent-then-newline-and-indent)
		      (define-key m (kbd "C-M-h") 'backward-kill-word)
		      (define-key m (kbd "C-c l") "lambda")
		      (define-key m [S-f7] 'ruby-compilation-this-buffer)
		      (define-key m [f7] 'ruby-compilation-this-test)
		      (define-key m [f8] 'recompile))))



;; (global-rbenv-mode)
(setq rbenv-modeline-function 'rbenv--modeline-plain)

;; enh robde mode
(add-hook 'ruby-mode 'robe-mode)
;; (add-hook 'robe-mode-hook 'ac-robe-setup)
;; e M-x robe-start.
(push 'company-robe company-backends)


;; js2-ac
;; (add-hook 'js2-mode-hook 'ac-js2-mode)
(require 'jquery-doc)
(add-hook 'js2-mode-hook 'jquery-doc-setup)

;;(set-face-background 'highlight-indentation-face "#e3e3d3")
;;(set-face-backgrocund 'highlight-indentation-current-ccolumn-face "#c3b3b3")

;; multiple-cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/macccrk-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-cprevious-like-this)
(global-set-key (kbd "C-c C-<") 'mc/macrk-all-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

;;==========
;;Go mode:
;;(add-to-list 'load-path "~/.emacs.d/go")
; (add-to-list 'load-path "~/.emacs.d/go-mode.el")
; (require 'go-mode)
; (add-hook 'before-save-hook 'gofmt-before-save)
; ; go-import-add is bound to C-c C-a by default
; (add-hook 'go-mode-hook '(lambda ()
; 			   (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
; ; Now we can use C-c C-r to remove all unused imports, or C-u C-c C-r to comment out unused imports.
; (add-hook 'go-mode-hook '(lambda ()
; 			   (local-set-key (kbd "C-c C-g") 'go-goto-imports)))
; ; format without save.
; (add-hook 'go-mode-hook '(lambda ()
; 			   (local-set-key (kbd "C-c C-f") 'gofmt)))
; (add-hook 'before-save-hook 'gofmt-before-save)
; ; documentation
; (add-hook 'go-mode-hook '(lambda ()
; 			   (local-set-key (kbd "C-c C-k") 'godoc)))
; ; godef is a really nifty tool that parses go code
; ; and enables you to quickly jump the definition of any symbol or read its description
; ; go-mode provides two functions for interacting
; ; with godef: godef-describe and godef-jump.
;
; (load "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")
; (add-hook 'go-mode-hook 'go-oracle-mode)
;
; ;; C-c C-o <       go-oracle-callers
; ;; C-c C-o >       go-oracle-callees
; ;; C-c C-o c       go-oracle-peers
; ;; C-c C-o d       go-oracle-definition
; ;; C-c C-o f       go-oracle-freevars
; ;; C-c C-o g       go-oracle-callgraph
; ;; C-c C-o i       go-oracle-implements
; ;; C-c C-o p       go-oracle-pointsto
; ;; C-c C-o r       go-oracle-referrers
; ;; C-c C-o s       go-oracle-callstack
; ;; C-c C-o t       go-oracle-describe
;
; (add-to-list 'load-path "~/gocode/src/github.com/dougm/goflymake")
; (require 'go-flymake)
;
; (require 'company)                                   ; load company mode
; (require 'company-go)                                ; load company mode go backend
;
; (add-hook 'go-mode-hook 'company-mode)
; (add-hook 'go-mode-hook (lambda ()
; 			  (set (make-local-variable 'company-backends) '(company-go))
; 			  (company-mode)))

;; (require 'auto-complete-config)
;; (setq ac-auto-start t)
;; (add-hook 'js2-mode-hook 'ac-js2-mode)

(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)

'(js3-auto-indent-p t)         ; it's nice for commas to right themselves.
'(js3-enter-indents-newline t) ; don't need to push tab before typing
'(js3-indent-on-enter-key t)   ; fix indenting before moving on

(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(require 'gist)


;; irony mode

(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(eval-after-load 'company
		 '(add-to-list 'company-backends 'company-irony))

;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;; Tern:

(add-to-list 'load-path "/home/rscnt/Packages/tern/emacs")
(autoload 'tern-mode "tern.el" nil t)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(add-to-list 'company-backends 'company-tern)

(require 'helm-config)
(helm-mode 1)
(helm-autoresize-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)


(add-hook 'before-save-hook 'whitespace-cleanup)

(load "pandoc-mode")
(add-hook 'markdown-mode-hook 'pandoc-mode)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

(autoload 'markdown-mode "markdown-mode"
	  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")
;; init.el ends here.
