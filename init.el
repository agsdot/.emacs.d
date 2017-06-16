;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(let ((file-name-handler-alist nil))
  ;; https://www.reddit.com/r/emacs/comments/56fvgd/is_there_a_way_to_stop_emacs_from_adding_the/
  (setq package--init-file-ensured t)
  ;; https://labo.olivierdelort.net/bricewge/dotfiles-emacs/blob/9c32a1232708137f7e247c8ceef79826ff9ff933/.emacs.d/init.el
  (if (file-exists-p "~/.emacs.d/config/emacs-init.elc")
      (progn
        (add-to-list 'load-path "~/.emacs.d/config/")
        (load "~/.emacs.d/config/emacs-init.elc"))
    (progn
      (org-babel-load-file "~/.emacs.d/config/emacs-init.org")
      (byte-compile-file "~/.emacs.d/config/emacs-init.el")))
)
;;https://emacs.stackexchange.com/questions/2286/what-can-i-do-to-speed-up-my-start-up
;;To test emacs startup speed do the following in a terminal:
;;time emacs --eval '(save-buffers-kill-terminal)'
