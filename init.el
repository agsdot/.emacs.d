(require 'package)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ;; ("org" . "http://orgmode.org/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(eval-when-compile (package-initialize))

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(require 'use-package)

(let ((default-directory "~/.emacs.d/custom/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(use-package esup
  :ensure esup)

;;;;;
(defcustom dotemacs-cache-directory (concat user-emacs-directory ".cache/")
  "The storage location for various persistent files.")

;; Backup Files location
;; https://github.com/bling/dotemacs/blob/master/config/init-core.el
(setq backup-directory-alist
  `((".*" . ,(concat dotemacs-cache-directory "backups")))
  auto-save-file-name-transforms
  `((".*" ,(concat dotemacs-cache-directory "backups") t))
  auto-save-list-file-prefix
  (concat dotemacs-cache-directory "auto-save-list/saves-"))

(use-package f
  :ensure f
  :init
  (unless (f-exists? dotemacs-cache-directory)
    (f-mkdir dotemacs-cache-directory)))

;; Dont display logo at startup
(setq inhibit-startup-message t)

;; How do I change the scratch message in Emacs?
;; http://stackoverflow.com/a/1498292/2741455
(setq initial-scratch-message ";; This is the Emacs Scratch Buffer")

;; Closing all other buffers in Emacs
;; http://stackoverflow.com/a/3417473/2741455
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer
    (delq (current-buffer)
      (remove-if-not 'buffer-file-name (buffer-list)))))

;; Don't automatically add newline to end of file
(setq mode-require-final-newline nil)

;; y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Don't automatically add newline to end of file
(setq mode-require-final-newline nil)

(use-package dired-x)

(use-package undo-tree
  :ensure undo-tree
  :config
    (global-undo-tree-mode t))

(use-package cross-platform-copy-paste)

(use-package recentf
  :ensure recentf
  :config
    (progn
      (recentf-mode 1)
      (setq recentf-max-menu-items 25)
      (setq recentf-save-file (concat dotemacs-cache-directory "recentf"))))

(setq-default tab-width 2)
(setq-default indent-tabs-mode nil) ;; seems to affect autocomplete modes

;; Turn off ding
(setq visible-bell 1)

;; aesthetics now ;;

(use-package smartparens
  :ensure t
  :config
    (progn
      (require 'smartparens-config)
      (smartparens-global-mode 1)))

(line-number-mode t)
(column-number-mode t)
(global-linum-mode t)

;; (use-package whitespace)
;;   ;; (global-whitespace-mode)
;;   (setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))
;;   (setq whitespace-display-mappings
;;     '((space-mark 32 [183] [46])
;;       (tab-mark 9 [9655 9] [92 9])))

;; (use-package gotham-theme)

(use-package zenburn-theme
  :ensure zenburn-theme)

;; navigation now ;;

(use-package neotree
  :ensure neotree)

(use-package smex
  :ensure smex
  :config
    (progn
      (smex-initialize)
      (setq smex-save-file (concat dotemacs-cache-directory "smex-items"))
      (global-set-key (kbd "M-x") 'smex)
      (global-set-key (kbd "M-X") 'smex-major-mode-commands)
      ;; This is your old M-x.
      (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)))

(use-package grizzl
  :ensure grizzl)

;; https://github.com/krobertson/emacs.d/blob/master/packages.el
(use-package projectile
  :ensure projectile
  :init (projectile-global-mode 1)
  ;; :bind (("s-p" . projectile-find-file)
  ;;        ("s-b" . projectile-switch-to-buffer)
  ;;        ("s-F" . projectile-ag))
  :config
  (progn
    (setq projectile-known-projects-file (concat dotemacs-cache-directory "projectile-bookmarks.eld"))
    ;; (setq projectile-enable-caching t)
    ;; (setq projectile-cache-file (concat dotemacs-cache-directory "projectile.cache"))
    (setq projectile-require-project-root nil)
    (setq projectile-completion-system 'grizzl)))
    ;; (setq projectile-globally-ignored-files
    ;;   (append projectile-globally-ignored-files
    ;;   '(
    ;;     ;; continuum import files
    ;;     "*.cntmp" )))
    ;; (add-to-list 'projectile-globally-ignored-files ".DS_Store")))

(use-package ace-jump-mode
  :ensure ace-jump-mode
  :config
    (progn
      (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)))

;; (use-package mouse
  ;; :config
  ;;   (progn
      ;; (xterm-mouse-mode t)
  ;;     (defun track-mouse (e))
  ;;     (setq mouse-sel-mode t))
;; )

(use-package menu-bar
  :config
  (menu-bar-mode -1))

;; ido mode configs
;; (use-package ido
;;   :ensure ido
;;   :init
;;   (ido-mode t))
(use-package ido-vertical-mode
  :ensure ido-vertical-mode
  :init
  (ido-vertical-mode t))

;; https://github.com/bdd/.emacs.d/blob/master/packages.el
(use-package flx-ido
  :ensure flx-ido
  :init
  (flx-ido-mode 1)
  :config
  (progn
    (setq flx-ido-threshhold 1000)
    (setq gc-cons-threshold 20000000)))

(use-package saveplace
  :ensure saveplace
  :init
  (setq-default save-place t)
  :config
  (setq save-place-forget-unreadable-files nil)
  ;; Try to make emacsclient play nice with saveplace
  ;; http://www.emacswiki.org/emacs/EmacsClient#toc35
  (setq server-visit-hook (quote (save-place-find-file-hook)))
  ;; rename this save file....
  (setq save-place-file "~/.emacs.d/.cache/saved-places"))

  ;; (setq save-place-file (concat dotemacs-cache-directory "saved-places"))
  ;; (setq smex-save-file (concat dotemacs-cache-directory "smex-items"))
  ;; (setq save-place-file (concat dotemacs-cache-directory "saved-places"))

;; coding ;;

;; enable seeing of git diffs
(use-package git-gutter
  :ensure git-gutter
  :init
    global-git-gutter-mode +1)

(use-package magit
  :ensure magit
  :config
    (progn
      ;; http://whattheemacsd.com/setup-magit.el-01.html
      (defadvice magit-status (around magit-fullscreen activate)
        (window-configuration-to-register :magit-fullscreen)
        ad-do-it
        (delete-other-windows))
      (defun magit-quit-session ()
        "Restores the previous window configuration and kills the magit buffer"
        (interactive)
        (kill-buffer)
        (jump-to-register :magit-fullscreen))))

(use-package web-mode
  :ensure web-mode
  :config
    (progn
      (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
      (add-to-list 'auto-mode-alist '("\\.gsp?\\'" . web-mode))))

(use-package js2-mode
  :ensure js2-mode
  :config
    (progn
      (add-to-list 'auto-mode-alist '("\\.js?\\'" . js2-mode))))

(use-package groovy-mode
  :ensure groovy-mode
  :config
    (progn
      (autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
      (add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
      (add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))
      (add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))))

(use-package lua-mode
  :ensure lua-mode
  :config
    (progn
      (add-to-list 'auto-mode-alist '("\\.lua?\\'" . js2-mode))))

(use-package vimrc-mode
  :ensure vimrc-mode
  :config
    (progn
      (add-to-list 'auto-mode-alist '(".vim\\(rc\\)?$" . vimrc-mode))))

(use-package drag-stuff
  :ensure drag-stuff
  :config
    (progn
      (drag-stuff-global-mode t)))

;; http://stackoverflow.com/a/15310340/2741455
;; How to set defcustom variable
(use-package linum-relative
  :ensure linum-relative
  :init
    (setq linum-relative-format "%3s ")
    (setq linum-relative-current-symbol ""))

(cond ((executable-find "pt")
        (use-package pt
          :ensure pt) ;; https://github.com/bling/pt.el
        (defalias 'my-search-util 'projectile-pt))  ;; seems pretty fast (faster than ag? maybe...dunno), but it's written in Go!
      ((executable-find "ag")
        (use-package ag
          :ensure ag) ;; https://github.com/Wilfred/ag.el
        (defalias 'my-search-util 'projectile-ag))  ;; on the website, it said faster than ack
      ((executable-find "ack")
        (use-package ack-and-a-half
          :ensure ack) ;; https://github.com/jhelwig/ack-and-a-half
        (defalias 'my-search-util 'projectile-ack))  ;; faster than grep
      ((executable-find "grep")
        (defalias 'my-search-util 'projectile-grep)))

;; evil mode setup ;;;
(setq evil-want-C-u-scroll t)
(setq evil-want-C-w-in-emacs-state t)
(setq evil-default-cursor t)

;; https://github.com/nathantypanski/emacs.d/blob/master/config/my-evil.el
;; In order to work properly, we need to load evil-leader-mode before we load
;; evil-mode.
;; (use-package evil-leader
;;   :commands (evil-leader-mode global-evil-leader-mode)
;;   :ensure evil-leader
;;   :requires (ace-jump-mode undo-tree)
;;   :demand evil-leader
;;   :init
;;   (progn
;;     (evil-leader/set-leader ",")
;;     (global-evil-leader-mode t))
;;   :config
;;   (progn
;;     (evil-leader/set-key
;;       "b" 'buffer-menu
;;       "f" 'my-search-util
;;       "l" 'linum-relative-toggle
;;       "nf" 'neotree-find
;;       "nt" 'neotree-toggle
;;       "p" 'projectile-find-file
;;       "r" 'recentf-open-files
;;       "/" 'evilnc-comment-or-uncomment-lines
;;       "<down>" 'drag-stuff-down
;;       "<up>" 'drag-stuff-up)))

;; Here's what we've all been waiting for.
;; Recreate Vim inside Emacs.
(use-package evil
  :ensure evil
  :config
  (progn

    (evil-mode 1)
    (setq evil-want-C-w-in-emacs-state t)
    ;; (setq evil-search-module        'isearch)
    ;; (setq evil-magic                'very-magic)
    ;; (setq evil-emacs-state-cursor   '("#dfaf8f" box))
    ;; (setq evil-normal-state-cursor  '("#f8f893" box))
    ;; (setq evil-insert-state-cursor  '("#f8f893" bar))
    ;; (setq evil-replace-state-cursor '("#cc9393" box))
    ;; (setq evil-want-fine-undo t)
    ;; (setq evil-want-change-word-to-end t)
    ;; Use a semicolon instead of the colon to get into command mode
    (define-key evil-normal-state-map ";" 'evil-ex)
    (define-key evil-normal-state-map ":" 'smex)

    (define-key evil-normal-state-map (kbd "C-<down>") 'drag-stuff-down)
    (define-key evil-normal-state-map (kbd "C-<up>") 'drag-stuff-up)
    
    ;; Make evil-mode up/down operate in screen lines instead of logical lines
    ;; (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
    ;; (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
    (define-key evil-motion-state-map "j"           'evil-next-visual-line)       ;; in my old configs
    (define-key evil-motion-state-map "k"           'evil-previous-visual-line)   ;; in my old configs
    ;; https://stackoverflow.com/questions/20882935/how-to-move-between-visual-lines-and-move-past-newline-in-evil-mode
    ;; Make horizontal movement cross lines
    (setq-default evil-cross-lines t)

    (use-package evil-leader
      :ensure evil-leader
      :config
      (progn
        (global-evil-leader-mode t)
        (evil-leader/set-leader ",")
        (evil-leader/set-key
          "b" 'buffer-menu
          "f" 'my-search-util
          "l" 'linum-relative-toggle
          "nf" 'neotree-find
          "nt" 'neotree-toggle
          "p" 'projectile-find-file
          "r" 'recentf-open-files
          "/" 'evilnc-comment-or-uncomment-lines
          "<down>" 'drag-stuff-down
          "<up>" 'drag-stuff-up)))
    
    (use-package evil-nerd-commenter
      :ensure evil-nerd-commenter
      :commands (evilnc-comment-or-uncomment-lines)
      :config
        (progn
          (evilnc-default-hotkeys)))

    (use-package evil-matchit
      :ensure evil-matchit
      :commands evilmi-jump-items
      :init
      (progn
        (setq global-evil-matchit-mode t)
        (define-key evil-normal-state-map "%" 'evilmi-jump-items)))

    (use-package evil-surround
      :ensure evil-surround
      :config
      (progn
        (global-evil-surround-mode 1)))

    (use-package evil-jumper
      :ensure evil-jumper
      :config
        (progn
          (setq evil-jumper-auto-center t)
          (setq evil-jumper-file (concat dotemacs-cache-directory "evil-jumps"))
          (setq evil-jumper-auto-save-interval 120)
          (setq evil-jumper-max-length 10)))

    (use-package evil-numbers
      :ensure evil-numbers
      :config
        (progn
          (define-key evil-normal-state-map (kbd "C-<right>") 'evil-numbers/inc-at-pt)
          (define-key evil-normal-state-map (kbd "C-<left>") 'evil-numbers/dec-at-pt)))
    
    (use-package powerline-evil
      :ensure powerline-evil
      :config
        (progn
          (powerline-evil-vim-theme)))

    (evil-set-initial-state 'magit-status-mode 'emacs)
    (evil-set-initial-state 'magit-log-edit-mode 'emacs)

    ;; (evil-set-initial-state 'flycheck-error-list-mode 'normal)
    ;; (evil-set-initial-state 'git-commit-mode 'insert)
    ;; (evil-set-initial-state 'shell-mode 'emacs)
    ;; (evil-set-initial-state 'esup-mode 'emacs)
    ;; (evil-set-initial-state 'diff-mode 'emacs)
    ;; (evil-set-initial-state 'term-mode 'emacs)
    ;; (evil-set-initial-state 'multi-term-mode 'emacs)

    (use-package key-chord
      :ensure key-chord
      :diminish key-chord-mode
      :config
      (progn
        (key-chord-mode 1)
        ;; from http://bbbscarter.wordpress.com/category/coding/emacs/
        (setq key-chord-two-keys-delay 0.2)
        (key-chord-define evil-insert-state-map "kj" 'evil-normal-state)))
    
    ;; (evil-define-text-object my-evil-next-match (count &optional beg end type)
    ;;   "Select next match."
    ;;   (evil-ex-search-previous 1)
    ;;   (evil-ex-search-next count)
    ;;   (list evil-ex-search-match-beg evil-ex-search-match-end))

    ;; (evil-define-text-object my-evil-previous-match (count &optional beg end type)
    ;;   "Select previous match."
    ;;   (evil-ex-search-next 1)
    ;;   (evil-ex-search-previous count)
    ;;   (list evil-ex-search-match-beg evil-ex-search-match-end))

    ;; (define-key minibuffer-local-map [escape] 'my-minibuffer-keyboard-quit)
    ;; (define-key minibuffer-local-ns-map [escape] 'my-minibuffer-keyboard-quit)
    ;; (define-key minibuffer-local-completion-map [escape] 'my-minibuffer-keyboard-quit)
    ;; (define-key minibuffer-local-must-match-map [escape] 'my-minibuffer-keyboard-quit)

    ;; (defun my-append-and-indent ()
    ;;   "Moves to end of line, enters insert mode, and also indents the line."
    ;;   (interactive)
    ;;   (evil-append-line 0)
    ;;   (indent-according-to-mode))

    ;; (define-key evil-insert-state-map (kbd "RET")        'evil-ret-and-indent)
    ;; (define-key evil-normal-state-map (kbd "RET")        'my-append-and-indent)
    ;; (define-key evil-normal-state-map (kbd "<S-return>") 'my-append-and-indent)
    (define-key evil-normal-state-map (kbd "C-w }") 'evil-window-rotate-downwards)
    (define-key evil-normal-state-map (kbd "C-w {") 'evil-window-rotate-upwards)

;;     (defun my-what-line ()
;;       "Get the line, without printing the word 'line' before it."
;;       (1+ (count-lines 1 (point))))

;;     (defun my-where-beginning-of-visual-line ()
;;       "Calculate the difference between the beginning
;; of the current visual line and point."
;;       (interactive)
;;       (let ((old-point (point))
;;             (bovl (save-excursion (beginning-of-visual-line)
;;                                   (point))))
;;         (- old-point bovl)))

    ;; (defun my-current-line-is-empty ()
    ;;   (save-excursion (beginning-of-line) (looking-at "\\s-+$")))

    ;; (defun my-delete-trailing-whitespace-at-line ()
    ;;   "Delete trailing whitespace on the current line only."
    ;;   (interactive)
    ;;   (let ((begin (line-beginning-position))
    ;;         (end   (line-end-position)))
    ;;     (delete-trailing-whitespace begin end)))

    ;; (defun my-electric-append-with-indent (count &optional vcount)
    ;;   "Indent the current line if it is empty. Otherwise, just do a normal append-line."
    ;;   (interactive "p")
    ;;   (if (and (= (point) (line-beginning-position))
    ;;            (my-is-this-line-empty))
    ;;       (indent-according-to-mode))
    ;;   (evil-append-line count vcount))

    ;; exiting insert mode -> delete trailing whitespace
    ;; (remove-hook 'evil-insert-state-exit-hook 'my-exit-insert-state)

    ;; (define-key evil-insert-state-map (kbd "<S-backspace>")
    ;;   'my-backward-delete-word)
    ;; (define-key evil-insert-state-map (kbd "<S-return>")
    ;;   'electric-indent-just-newline)
    ;; (define-key evil-normal-state-map (kbd "<S-return>")
    ;;   'electric-indent-just-newline)

    ;; (define-key evil-normal-state-map (kbd "SPC a") 'ag)
    ;; (define-key evil-normal-state-map (kbd "SPC SPC") 'helm-M-x)

    ;; (define-key evil-normal-state-map (kbd "C-q")   'universal-argument)

    (define-key evil-normal-state-map (kbd "C-h")   'evil-window-left)
    (define-key evil-normal-state-map (kbd "C-j")   'evil-window-down)
    (define-key evil-normal-state-map (kbd "C-k")   'evil-window-up)
    (define-key evil-normal-state-map (kbd "C-l")   'evil-window-right)
    ;; (define-key evil-normal-state-map (kbd "-") (kbd "dd"))

    ;(define-key evil-normal-state-map "a"           'evil-append)
    ;(define-key evil-normal-state-map "A"           'my-electric-append-with-indent)
    ;(define-key evil-normal-state-map "$"           'my-smart-end)
    ;(define-key evil-normal-state-map "0"           'my-smart-home)

    ;; (define-key evil-motion-state-map "h"           'evil-backward-char)
    ;; (define-key evil-motion-state-map "j"           'evil-next-visual-line)       ;; in my old configs
    ;; (define-key evil-motion-state-map "k"           'evil-previous-visual-line)   ;; in my old configs
    ;; (define-key evil-motion-state-map "l"           'evil-forward-char)
    ;; (define-key evil-motion-state-map "$"           'evil-end-of-line)
    ;; (define-key evil-motion-state-map "0"           'evil-beginning-of-line)

    ;; (define-key evil-normal-state-map "/"           'evil-search-forward)
    ;; (define-key evil-normal-state-map (kbd "SPC /") 'helm-swoop)
    ;; (define-key evil-motion-state-map "/"           'evil-search-forward)
    ;; (define-key evil-normal-state-map (kbd "Y") (kbd "y$"))

    (evil-ex-define-cmd "Q"  'evil-quit)
    (evil-ex-define-cmd "Qa" 'evil-quit-all)
    (evil-ex-define-cmd "QA" 'evil-quit-all)

    ;; (evil-define-key 'motion python-mode-map "]]" 'python-nav-forward-block)
    ;; (evil-define-key 'motion python-mode-map "][" 'python-nav-end-of-block)
    ;; (evil-define-key 'motion python-mode-map "[[" 'python-nav-backward-block)
    ;; (evil-define-key 'motion python-mode-map "[]" 'my-python-nav-backward-end-of-block)
    ;; (evil-define-key 'motion python-mode-map "[(" 'evil-previous-open-paren)
    ;; (evil-define-key 'motion python-mode-map "])" 'evil-next-close-paren)
    ;; (evil-define-key 'motion python-mode-map "[{" 'evil-previous-open-brace)
    ;; (evil-define-key 'motion python-mode-map "]}" 'evil-next-close-brace)
    ))


;; setup extra keybindings ;;
;; Bind DEL and = keys to scrolling up and down
;; https://stackoverflow.com/questions/8483182/evil-mode-best-practice
(define-key evil-normal-state-map (kbd "DEL") (lambda ()
  (interactive)
  (previous-line 10)
  (evil-scroll-line-up 10)))

(define-key evil-normal-state-map (kbd "=") (lambda ()
  (interactive)
  (next-line 10)
  (evil-scroll-line-down 10)))

;; (windmove-default-keybindings)

;; (use-package windmove
;;   :ensure t
;;   :config (windmove-default-keybindings))