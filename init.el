(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(color-theme
    solarized-theme 
    monokai-theme 
    zenburn-theme
    nzenburn-theme
    color-theme-sanityinc-tomorrow 
    base16-theme

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
    rainbow-delimiters

    evil
    evil-leader
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

(load-theme 'sanityinc-tomorrow-eighties t) 

;; Display line and column numbers in mode line.
(line-number-mode t)
(column-number-mode t)
(global-linum-mode t)

;; Disable the creation of backup files.
(setq backup-inhibited t)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; fast file management
(projectile-global-mode)

;; enable evil nerd commenter commenting, works in emacs mode and vim/evil mode 
(evilnc-default-hotkeys)

;; enable sidebar file managers 
(autoload 'dirtree "dirtree" "Add directory to tree view" t)
(require 'project-explorer) 

;; enable seeing of git diffs
(require 'git-gutter)
(global-git-gutter-mode +1)

;; y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; (require 'no-easy-keys)
;;(load "no-easy-keys.el")
;;(no-easy-keys 0)

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

;; FIX SHIFT-UP
;; from http://stackoverflow.com/questions/10871745/shift-up-arrow-doesnt-highlight-text-emacs-iterm2
;; and http://lists.gnu.org/archive/html/help-gnu-emacs/2011-05/msg00211.html
(define-key input-decode-map "\e[1;2A" [S-up])
(if (equal "xterm" (tty-type))
    (define-key input-decode-map "\e[1;2A" [S-up]))
(defadvice terminal-init-xterm (after select-shift-up activate)
  (define-key input-decode-map "\e[1;2A" [S-up]))

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

(electric-pair-mode 1)
(show-paren-mode 1)

(setq-default tab-width 2)
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)

;; Fast cursor movement in vertical direction with Meta.
(global-set-key (kbd "M-n") (lambda () (interactive) (next-line 5)))
(global-set-key (kbd "M-p") (lambda () (interactive) (previous-line 5)))

(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
;; (global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-c C-p") 'projectile-find-file)
(global-set-key (kbd "C-c C-f") 'projectile-grep)

(require 'whitespace)
(global-whitespace-mode)
(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))
(setq whitespace-display-mappings
      '((space-mark 32 [183] [46]) 
        (tab-mark 9 [9655 9] [92 9])))

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Dont display logo at startup
(setq inhibit-startup-message t)

;; Add powerline to replace default emacs bar
(require 'powerline)
(powerline-default-theme)
(setq powerline-arrow-shape 'arrow)   ;; the default
(setq powerline-arrow-shape 'curve)   ;; give your mode-line curves
(setq powerline-arrow-shape 'arrow14) ;; best for small fonts
(setq powerline-default-theme nil)

;; http://stackoverflow.com/questions/2592095/how-do-i-create-an-empty-file-in-emacs
;; (add-hook 'find-file-hooks 'assume-new-is-modified)
;; (defun assume-new-is-modified ()
;;   (when (not (file-exists-p (buffer-file-name)))
;;     (set-buffer-modified-p t)))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.gsp?\\'" . web-mode))

;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(require 'groovy-mode)
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;;;vim navigation
(require 'evil)
(defalias 'em 'evil-mode)
(evil-mode 1)
(global-evil-leader-mode)
; Make horizontal movement cross lines
(setq-default evil-cross-lines t)
; (after "evil-leader-autoloads"
    (evil-leader/set-leader ",")
    (evil-leader/set-key
      "f" 'projectile-grep
      "p" 'projectile-find-file
      "r" 'recentf-open-files
      "b" 'buffer-menu
)
