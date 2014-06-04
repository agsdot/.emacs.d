(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(solarized-theme monokai-theme evil-nerd-commenter dirtree projectile git-gutter undo-tree ido web-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
  
(load-theme 'monokai t)

;; Display line and column numbers in mode line.
(line-number-mode t)
(column-number-mode t)
(global-linum-mode t)

;; Disable the creation of backup files.
(setq backup-inhibited t)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; enable evil nerd commenter commenting 
(evilnc-default-hotkeys)

;; enable dirtree 
(autoload 'dirtree "dirtree" "Add directory to tree view" t)

;; fast file management
(projectile-global-mode)

;; enable seeing of git diffs
(require 'git-gutter)
(global-git-gutter-mode +1)

;; y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

(require 'evil)
(evil-mode 0)
; Make horizontal movement cross lines                          
(setq-default evil-cross-lines t)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.gsp?\\'" . web-mode))
