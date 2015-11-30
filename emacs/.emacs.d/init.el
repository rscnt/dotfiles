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

(set-face-attribute 'default nil :font "Source Code Pro for Powerline 8")
(set-frame-font "Source Code Pro for Powerline 8" nil t)

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

(require 'ido)
(ido-mode t)

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
; (yas-reload-all)
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
(add-to-list 'load-path "~/.emacs.d/go-mode.el")
(require 'go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)
; go-import-add is bound to C-c C-a by default
(add-hook 'go-mode-hook '(lambda ()
			   (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)))
; Now we can use C-c C-r to remove all unused imports, or C-u C-c C-r to comment out unused imports.
(add-hook 'go-mode-hook '(lambda ()
			   (local-set-key (kbd "C-c C-g") 'go-goto-imports)))
; format without save.
(add-hook 'go-mode-hook '(lambda ()
			   (local-set-key (kbd "C-c C-f") 'gofmt)))
(add-hook 'before-save-hook 'gofmt-before-save)
; documentation
(add-hook 'go-mode-hook '(lambda ()
			   (local-set-key (kbd "C-c C-k") 'godoc)))
; godef is a really nifty tool that parses go code
; and enables you to quickly jump the definition of any symbol or read its description
; go-mode provides two functions for interacting
; with godef: godef-describe and godef-jump.

;(load "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")
;(add-hook 'go-mode-hook 'go-oracle-mode)

;; C-c C-o <       go-oracle-callers
;; C-c C-o >       go-oracle-callees
;; C-c C-o c       go-oracle-peers
;; C-c C-o d       go-oracle-definition
;; C-c C-o f       go-oracle-freevars
;; C-c C-o g       go-oracle-callgraph
;; C-c C-o i       go-oracle-implements
;; C-c C-o p       go-oracle-pointsto
;; C-c C-o r       go-oracle-referrers
;; C-c C-o s       go-oracle-callstack
;; C-c C-o t       go-oracle-describe

;(add-to-list 'load-path "~/gocode/src/github.com/dougm/goflymake")
;(require 'go-flymake)

(require 'company)                                   ; load company mode
(require 'company-go)                                ; load company mode go backend

(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook (lambda ()
			  (set (make-local-variable 'company-backends) '(company-go))
			  (company-mode)))

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

(add-to-list 'load-path "/home/rscnt/development/tern/emacs")
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

(defun rst-validate-buffer ()
  "Tests to see if buffer is valid reStructured Text."
  (interactive)
  (if (eq major-mode 'rst-mode)         ; only runs in rst-mode
      (let ((name (buffer-name))
	    (filename (buffer-file-name)))
	(cond
	 ((not (file-exists-p "/usr/bin/rst2pseudoxml")) ; check that the program used to process file is present
	      (error "Docutils programs not available."))
	 ((not (file-exists-p filename)) ; check that the file of the buffer exists
	      (error "Buffer '%s' is not visiting a file!" name))
	 (t                             ; ok, process the file, producing pseudoxml output
	  (let* ((pseudoxml (split-string (shell-command-to-string (concat "rst2pseudoxml " filename)) "\n"))
		 (firstline (car pseudoxml)) ; take first line of output
		 (lastline (nth (- (length pseudoxml) 2) pseudoxml))) ; take last line of output text
	    (if (or (string-match "ERROR/" firstline)
		    (string-match "WARNING/" firstline)
		    ;; for reasons I don't understand, sometimes the warnings/errors are at the end of output
		    (string-match "ERROR/" lastline)
		    (string-match "WARNING/" lastline))
		(progn (ding)
		       (message "There's an problem in this buffer."))
	      (message "Buffer is valid reStructured Text."))))))))

(add-hook 'rst-mode-hook
	  (lambda()
	    (add-hook 'after-save-hook 'rst-validate-buffer)))

;; find aspell and hunspell automatically
(cond
 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US")))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  (setq ispell-extra-args '("-d en_US")))
 )

					; if (aspell installed) { use aspell}
;; else if (hunspell installed) { use hunspell }
;; whatever spell checker I use, I always use English dictionary
;; I prefer use aspell because:
;; 1. aspell is older
;; 2. looks Kevin Atkinson still get some road map for aspell:
;; @see http://lists.gnu.org/archive/html/aspell-announce/2011-09/msg00000.html
(defun flyspell-detect-ispell-args (&optional RUN-TOGETHER)
  "if RUN-TOGETHER is true, spell check the CamelCase words"
  (let (args)
    (cond
     ((string-match  "aspell$" ispell-program-name)
      ;; force the English dictionary, support Camel Case spelling check (tested with aspell 0.6)
      (setq args (list "--sug-mode=ultra" "--lang=en_US"))
      (if RUN-TOGETHER
	  (setq args (append args '("--run-together" "--run-together-limit=5" "--run-together-min=2")))))
     ((string-match "hunspell$" ispell-program-name)
      (setq args nil)))
    args
    ))

(cond
 ((executable-find "aspell")
  (setq ispell-program-name "aspell"))
 ((executable-find "hunspell")
  (setq ispell-program-name "hunspell")
  ;; just reset dictionary to the safe one "en_US" for hunspell.
  ;; if we need use different dictionary, we specify it in command line arguments
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
	'(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8))))
 (t (setq ispell-program-name nil)))

;; ispell-cmd-args is useless, it's the list of *extra* arguments we will append to the ispell process when "ispell-word" is called.
;; ispell-extra-args is the command arguments which will *always* be used when start ispell process
(setq ispell-extra-args (flyspell-detect-ispell-args t))
;; (setq ispell-cmd-args (flyspell-detect-ispell-args))
(defadvice ispell-word (around my-ispell-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))

(defadvice flyspell-auto-correct-word (around my-flyspell-auto-correct-word activate)
  (let ((old-ispell-extra-args ispell-extra-args))
    (ispell-kill-ispell t)
    ;; use emacs original arguments
    (setq ispell-extra-args (flyspell-detect-ispell-args))
    ad-do-it
    ;; restore our own ispell arguments
    (setq ispell-extra-args old-ispell-extra-args)
    (ispell-kill-ispell t)
    ))

(require 'fill-column-indicator)
(define-globalized-minor-mode
  global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode t)

(require 'python-mode)
			; use IPython
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
					; use the wx backend, for both mayavi and matplotlib
(setq py-python-command-args
      '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
(setq py-force-py-shell-name-p t)

					; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)
					; don't split windows
(setq py-split-windows-on-execute-p nil)
					; try to automagically figure out indentation
(setq py-smart-indentation t)

;(autoload 'pymacs-apply "pymacs")
;(autoload 'pymacs-call "pymacs")
;(autoload 'pymacs-eval "pymacs" nil t)
;(autoload 'pymacs-exec "pymacs" nil t)
;(autoload 'pymacs-load "pymacs" nil t)
;(autoload 'pymacs-autoload "pymacs")

; ropemacs
;(require 'pymacs)
;(pymacs-load "ropemacs" "rope-")

; (autoload 'pymacs-apply "pymacs")
; (autoload 'pymacs-call "pymacs")
; (autoload 'pymacs-eval "pymacs" nil t)
; (autoload 'pymacs-exec "pymacs" nil t)
; (autoload 'pymacs-load "pymacs" nil t)
; (autoload 'pymacs-autoload "pymacs")

; ropemacs
; (require 'pymacs)
; (pymacs-load "ropemacs" "rope-")

(when (load "flymake" t)
     (defun flymake-pyflakes-init ()
       (let* ((temp-file (flymake-init-create-temp-buffer-copy
			  'flymake-create-temp-inplace))
      (local-file (file-relative-name
	       temp-file
	       (file-name-directory buffer-file-name))))
	 (list "pyflakes" (list local-file))))

     (add-to-list 'flymake-allowed-file-name-masks
	  '("\\.py\\'" flymake-pyflakes-init)))

(add-hook 'find-file-hook 'flymake-find-file-hook)

(defun my-flymake-show-help ()
  (when (get-char-property (point) 'flymake-overlay)
   (let ((help (get-char-property (point) 'help-echo)))
    (if help (message "%s" help)))))

(add-hook 'post-command-hook 'my-flymake-show-help)


(eval-after-load 'js2-mode
  '(progn
     (require 'js2-imenu-extras)

     ;; The code to record the class is identical to that for
     ;; Backbone so we just make an alias
     (defalias 'js2-imenu-record-react-class
       'js2-imenu-record-backbone-extend)

     (unless (loop for entry in js2-imenu-extension-styles
		   thereis (eq (plist-get entry :framework) 'react))
       (push '(:framework react
			  :call-re "\\_<React\\.createClass\\s-*("
			  :recorder js2-imenu-record-react-class)
	     js2-imenu-extension-styles))

     (add-to-list 'js2-imenu-available-frameworks 'react)
     (add-to-list 'js2-imenu-enabled-frameworks 'react)))

(defun modify-syntax-table-for-jsx ()
  (modify-syntax-entry ?< "(>")
  (modify-syntax-entry ?> ")<"))

(add-hook 'js2-mode-hook 'modify-syntax-table-for-jsx)

(eval-after-load 'js2-mode
  '(sp-local-pair 'js2-mode "<" ">"))

(setq magit-last-seen-setup-instructions "1.4.0")

(require 'solarized-theme)
(load-theme 'solarized-light t)
(require 'powerline)
(powerline-default-theme)

;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

(setq sml/no-confirm-load-theme t)
(setq sml/theme 'light)
(sml/setup)

(setq-default indent-tabs-mode nil)

;; init.el ends here.
