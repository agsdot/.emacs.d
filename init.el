;; https://www.reddit.com/r/emacs/comments/56fvgd/is_there_a_way_to_stop_emacs_from_adding_the/
(setq package--init-file-ensured t)
;; https://labo.olivierdelort.net/bricewge/dotfiles-emacs/blob/9c32a1232708137f7e247c8ceef79826ff9ff933/.emacs.d/init.el
(org-babel-load-file (expand-file-name "emacs-init.org" user-emacs-directory))
