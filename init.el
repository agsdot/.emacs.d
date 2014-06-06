(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(solarized-theme 
    monokai-theme 
    zenburn-theme
    nzenburn-theme

    ido
    ido-vertical-mode
    smex
    
    projectile 
    evil-nerd-commenter 
    crosshairs 
    dirtree 
    undo-tree 
    project-explorer
    powerline 
    ace-jump-mode    

    evil

    git-gutter
 
    web-mode
    groovy-mode
    )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(add-to-list 'load-path "~/.emacs.d/custom")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

(load-theme 'nzenburn t)

;; Display line and column numbers in mode line.
(line-number-mode t)
(column-number-mode t)
(global-linum-mode t)

;; Disable the creation of backup files.
(setq backup-inhibited t)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; enable evil nerd commenter commenting, works in emacs mode and vim/evil mode 
(evilnc-default-hotkeys)

;;;vim navigation
(require 'evil)
(defalias 'em 'evil-mode)
(evil-mode 0)
; Make horizontal movement cross lines                          
(setq-default evil-cross-lines t)

;; enable sidebar file managers 
(autoload 'dirtree "dirtree" "Add directory to tree view" t)
(require 'project-explorer) 

;; fast file management
(projectile-global-mode)

;; enable seeing of git diffs
(require 'git-gutter)
(global-git-gutter-mode +1)

;; y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; (require 'no-easy-keys)
(load "no-easy-keys.el")
(no-easy-keys 0)

;; menu bar visibility
(menu-bar-mode -1)

;; ido mode configs
(require 'ido)
(require 'ido-vertical-mode)
(ido-mode t)
(ido-vertical-mode t)

(require 'smex) ; Not needed if you use package.el
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; C-x C-j opens dired with the cursor right on the file you're editing
(require 'dired-x)

;; Turn off ding
(setq visible-bell 1)

(require 'crosshairs)
(toggle-crosshairs-when-idle 30)

(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; copy/paste with C-c and C-v and C-x, check out C-RET too
(cua-mode)
;; cross platform copy paste
(load "cross-platform-copy-paste.el")

;; From http://ergoemacs.org/emacs/emacs_best_redo_mode.html
(require 'undo-tree)
(global-undo-tree-mode t)
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "C-z") 'undo) ; 【Ctrl+z】
(global-set-key (kbd "C-S-z") 'redo) ; 【Ctrl+Shift+z】;  Mac style
(global-set-key (kbd "C-y") 'redo) ; 【Ctrl+y】; Microsoft Windows style

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.gsp?\\'" . web-mode))

;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(require 'groovy-mode)
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

