;; init.el:

(require 'cask "~/.cask/cask.el")
(cask-initialize)
(require 'pallet)
(pallet-mode t)

(prefer-coding-system 'utf-8)



(setq default-buffer-file-coding-system 'utf-8)

 ;;add melpa repo.
;; ================
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  )

;;(add-hook 'after-init-hook 'global-company-mode)

;;=============
;; Flycheck mode.
(add-hook 'after-init-hook #'global-flycheck-mode)
(require 'flycheck-color-mode-line)
(eval-after-load "flycheck"
                 '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

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


;;============================
;;; Display
(tool-bar-mode 0)
(menu-bar-mode -1)

(setq inhibit-splash-screen t)
;;=============================

;;=============================
;; Linum
;;=============================
(require 'linum)
(add-hook 'after-init-hook #'linum-mode)

;;(require 'linum-relative)
;;=============================

;; Nyan mode.
;;=============================
(add-hook 'after-init-hook #'nyan-mode)
; (add-hook 'nyan-mode-hook #'nyan-start-animation)

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
;;(load-theme 'brin t)
(require 'solarized-theme)
;;(require 'color-theme)
;;(color-theme-initialize)
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/base16-emacs")
;;(load-theme 'brin t)

; (setq font-lock-maximum-decoration t)

(set-face-attribute 'default nil :font "Envy Code R 10")
(set-frame-font "Envy Code R 10" nil t)

;; smartparens

(require 'smartparens-config)
(require 'smartparens-ruby)
(smartparens-global-mode)
(show-smartparens-global-mode t)
(sp-with-modes '(rhtml-mode)
               (sp-local-pair "<" ">")
               (sp-local-pair "<%" "%>"))

;; ruby mode
(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))

(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(setq enh-ruby-bounce-deep-indent t)
(setq enh-ruby-hanging-brace-indent-level 2)

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
;; (global-rbenv-mode)
(setq rbenv-modeline-function 'rbenv--modeline-plain)

;; enh robde mode
(add-hook 'enh-ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)
;; e M-x robe-start.
;;(push 'company-robe company-backends)

;; js2-ac
(add-hook 'js2-mode-hook 'ac-js2-mode)
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

;;Go mode
(add-to-list 'load-path "~/.emacs.d/go/")
(add-to-list 'load-path "~/.emacs.d/go/go-mode.el/")
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

(load "$GOPATH/src/code.google.com/p/go.tools/cmd/oracle/oracle.el")
(add-hook 'go-mode-hook 'go-oracle-mode)

;C-c C-o <       go-oracle-callers
;C-c C-o >       go-oracle-callees
;C-c C-o c       go-oracle-peers
;C-c C-o d       go-oracle-definition
;C-c C-o f       go-oracle-freevars
;C-c C-o g       go-oracle-callgraph
;C-c C-o i       go-oracle-implements
;C-c C-o p       go-oracle-pointsto
;C-c C-o r       go-oracle-referrers
;C-c C-o s       go-oracle-callstack
;;C-c C-o t       go-oracle-describe

(require 'go-autocomplete)
(require 'auto-complete-config)

(setq ac-auto-start t)
(add-hook 'js2-mode-hook 'ac-js2-mode)

(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)


;;(require 'evil)
;;(evil-mode 1)

;; end of init.el
(put 'upcase-region 'disabled nil)

(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(sml/setup)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:foreground "#657b83" :background "#fdf6e3")))))



(setq default-buffer-file-coding-system 'utf-8)

;; add melpa repo.
;; ================
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  )

;;(add-hook 'after-init-hook 'global-company-mode)

;;=============
;; Flycheck mode.
(add-hook 'after-init-hook #'global-flycheck-mode)
(require 'flycheck-color-mode-line)
(eval-after-load "flycheck"
                 '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

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


;;============================
;;; Display
(tool-bar-mode 0)
(menu-bar-mode -1)

(setq inhibit-splash-screen t)
;;=============================

;;=============================
;; Linum
;;=============================
(require 'linum)
(add-hook 'after-init-hook #'linum-mode)

;;(require 'linum-relative)
;;=============================

;; Nyan mode.
;;=============================
(add-hook 'after-init-hook #'nyan-mode)
; (add-hook 'nyan-mode-hook #'nyan-start-animation)

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
;;(load-theme 'brin t)
(require 'solarized-theme)
;;(require 'color-theme)
;;(color-theme-initialize)
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/base16-emacs")
;;(load-theme 'brin t)

; (setq font-lock-maximum-decoration t)

(set-face-attribute 'default nil :font "Envy Code R 10")
(set-frame-font "Envy Code R 10" nil t)

;; smartparens

(require 'smartparens-config)
(require 'smartparens-ruby)
(smartparens-global-mode)
(show-smartparens-global-mode t)
(sp-with-modes '(rhtml-mode)
               (sp-local-pair "<" ">")
               (sp-local-pair "<%" "%>"))

;; ruby mode
(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))

(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(setq enh-ruby-bounce-deep-indent t)
(setq enh-ruby-hanging-brace-indent-level 2)

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
;; (global-rbenv-mode)
(setq rbenv-modeline-function 'rbenv--modeline-plain)

;; enh robde mode
(add-hook 'enh-ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)
;; e M-x robe-start.
;;(push 'company-robe company-backends)

;; js2-ac
(add-hook 'js2-mode-hook 'ac-js2-mode)
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

;;Go mode
(add-to-list 'load-path "~/.emacs.d/go/")
(add-to-list 'load-path "~/.emacs.d/go/go-mode.el/")
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

(load "$GOPATH/src/code.google.com/p/go.tools/cmd/oracle/oracle.el")
(add-hook 'go-mode-hook 'go-oracle-mode)

;C-c C-o <       go-oracle-callers
;C-c C-o >       go-oracle-callees
;C-c C-o c       go-oracle-peers
;C-c C-o d       go-oracle-definition
;C-c C-o f       go-oracle-freevars
;C-c C-o g       go-oracle-callgraph
;C-c C-o i       go-oracle-implements
;C-c C-o p       go-oracle-pointsto
;C-c C-o r       go-oracle-referrers
;C-c C-o s       go-oracle-callstack
;;C-c C-o t       go-oracle-describe

(require 'go-autocomplete)
(require 'auto-complete-config)

(setq ac-auto-start t)
(add-hook 'js2-mode-hook 'ac-js2-mode)

(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)


;;(require 'evil)
;;(evil-mode 1)

;; end of init.el
(put 'upcase-region 'disabled nil)

(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(sml/setup)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:foreground "#657b83" :background "#fdf6e3")))))



(setq default-buffer-file-coding-system 'utf-8)

;; add melpa repo.
;; ================
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  )

;;(add-hook 'after-init-hook 'global-company-mode)

;;=============
;; Flycheck mode.
(add-hook 'after-init-hook #'global-flycheck-mode)
(require 'flycheck-color-mode-line)
(eval-after-load "flycheck"
                 '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

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


;;============================
;;; Display
(tool-bar-mode 0)
(menu-bar-mode -1)

(setq inhibit-splash-screen t)
;;=============================

;;=============================
;; Linum
;;=============================
(require 'linum)
(add-hook 'after-init-hook #'linum-mode)

;;(require 'linum-relative)
;;=============================

;; Nyan mode.
;;=============================
(add-hook 'after-init-hook #'nyan-mode)
; (add-hook 'nyan-mode-hook #'nyan-start-animation)

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
;;(load-theme 'brin t)
(require 'solarized-theme)
;;(require 'color-theme)
;;(color-theme-initialize)
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/base16-emacs")
;;(load-theme 'brin t)

; (setq font-lock-maximum-decoration t)

(set-face-attribute 'default nil :font "Envy Code R 10")
(set-frame-font "Envy Code R 10" nil t)

;; smartparens

(require 'smartparens-config)
(require 'smartparens-ruby)
(smartparens-global-mode)
(show-smartparens-global-mode t)
(sp-with-modes '(rhtml-mode)
               (sp-local-pair "<" ">")
               (sp-local-pair "<%" "%>"))

;; ruby mode
(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))

(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(setq enh-ruby-bounce-deep-indent t)
(setq enh-ruby-hanging-brace-indent-level 2)

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
;; (global-rbenv-mode)
(setq rbenv-modeline-function 'rbenv--modeline-plain)

;; enh robde mode
(add-hook 'enh-ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)
;; e M-x robe-start.
;;(push 'company-robe company-backends)

;; js2-ac
(add-hook 'js2-mode-hook 'ac-js2-mode)
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

;;Go mode
(add-to-list 'load-path "~/.emacs.d/go/")
(add-to-list 'load-path "~/.emacs.d/go/go-mode.el/")
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

(load "$GOPATH/src/code.google.com/p/go.tools/cmd/oracle/oracle.el")
(add-hook 'go-mode-hook 'go-oracle-mode)

;C-c C-o <       go-oracle-callers
;C-c C-o >       go-oracle-callees
;C-c C-o c       go-oracle-peers
;C-c C-o d       go-oracle-definition
;C-c C-o f       go-oracle-freevars
;C-c C-o g       go-oracle-callgraph
;C-c C-o i       go-oracle-implements
;C-c C-o p       go-oracle-pointsto
;C-c C-o r       go-oracle-referrers
;C-c C-o s       go-oracle-callstack
;;C-c C-o t       go-oracle-describe

(require 'go-autocomplete)
(require 'auto-complete-config)

(setq ac-auto-start t)
(add-hook 'js2-mode-hook 'ac-js2-mode)

(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)


;;(require 'evil)
;;(evil-mode 1)

;; end of init.el
(put 'upcase-region 'disabled nil)

(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(sml/setup)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:foreground "#657b83" :background "#fdf6e3")))))



(setq default-buffer-file-coding-system 'utf-8)

;; add melpa repo.
;; ================
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  )

;;(add-hook 'after-init-hook 'global-company-mode)

;;=============
;; Flycheck mode.
(add-hook 'after-init-hook #'global-flycheck-mode)
(require 'flycheck-color-mode-line)
(eval-after-load "flycheck"
                 '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

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


;;============================
;;; Display
(tool-bar-mode 0)
(menu-bar-mode -1)

(setq inhibit-splash-screen t)
;;=============================

;;=============================
;; Linum
;;=============================
(require 'linum)
(add-hook 'after-init-hook #'linum-mode)

;;(require 'linum-relative)
;;=============================

;; Nyan mode.
;;=============================
(add-hook 'after-init-hook #'nyan-mode)
; (add-hook 'nyan-mode-hook #'nyan-start-animation)

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
;;(load-theme 'brin t)
(require 'solarized-theme)
;;(require 'color-theme)
;;(color-theme-initialize)
;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/base16-emacs")
;;(load-theme 'brin t)

; (setq font-lock-maximum-decoration t)

(set-face-attribute 'default nil :font "Envy Code R 10")
(set-frame-font "Envy Code R 10" nil t)

;; smartparens

(require 'smartparens-config)
(require 'smartparens-ruby)
(smartparens-global-mode)
(show-smartparens-global-mode t)
(sp-with-modes '(rhtml-mode)
               (sp-local-pair "<" ">")
               (sp-local-pair "<%" "%>"))

;; ruby mode
(autoload 'enh-ruby-mode "enh-ruby-mode" "Major mode for ruby files" t)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))

(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(setq enh-ruby-bounce-deep-indent t)
(setq enh-ruby-hanging-brace-indent-level 2)

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
;; (global-rbenv-mode)
(setq rbenv-modeline-function 'rbenv--modeline-plain)

;; enh robde mode
(add-hook 'enh-ruby-mode-hook 'robe-mode)
(add-hook 'robe-mode-hook 'ac-robe-setup)
;; e M-x robe-start.
;;(push 'company-robe company-backends)

;; js2-ac
(add-hook 'js2-mode-hook 'ac-js2-mode)
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

;;Go mode
(add-to-list 'load-path "~/.emacs.d/go/")
(add-to-list 'load-path "~/.emacs.d/go/go-mode.el/")
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

(load "$GOPATH/src/code.google.com/p/go.tools/cmd/oracle/oracle.el")
(add-hook 'go-mode-hook 'go-oracle-mode)

;C-c C-o <       go-oracle-callers
;C-c C-o >       go-oracle-callees
;C-c C-o c       go-oracle-peers
;C-c C-o d       go-oracle-definition
;C-c C-o f       go-oracle-freevars
;C-c C-o g       go-oracle-callgraph
;C-c C-o i       go-oracle-implements
;C-c C-o p       go-oracle-pointsto
;C-c C-o r       go-oracle-referrers
;C-c C-o s       go-oracle-callstack
;;C-c C-o t       go-oracle-describe

(require 'go-autocomplete)
(require 'auto-complete-config)

(setq ac-auto-start t)
(add-hook 'js2-mode-hook 'ac-js2-mode)

(add-hook 'js2-mode-hook 'skewer-mode)
(add-hook 'css-mode-hook 'skewer-css-mode)
(add-hook 'html-mode-hook 'skewer-html-mode)


;;(require 'evil)
;;(evil-mode 1)

;; end of init.el
(put 'upcase-region 'disabled nil)

(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

(sml/setup)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("756597b162f1be60a12dbd52bab71d40d6a2845a3e3c2584c6573ee9c332a66e" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:foreground "#657b83" :background "#fdf6e3")))))

(pallet-mode t)

;; init-el ends here
