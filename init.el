;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(let ((file-name-handler-alist nil))
  (org-babel-load-file "~/.emacs.d/config/emacs-init.org")
)
;;https://emacs.stackexchange.com/questions/2286/what-can-i-do-to-speed-up-my-start-up
;;To test emacs startup speed do the following in a terminal:
;;time emacs --eval '(save-buffers-kill-terminal)'
