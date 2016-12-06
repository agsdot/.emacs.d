Lets refactor this emacs to have:

use-package
org mode presentation


try to limit git submodules
cask? lets reduce the moving parts, limit for now.

Thoughts?  Do I need to declare emacs bankruptcy and clean slate?  No not really. I like my configs.

I guess its to super optimize  

  - make it possibly load faster  
    - https://www.reddit.com/r/emacs/comments/2md52c/what_configurations_should_be_put_or_avoided/  
  - make it spiffier with org mode  

Helpful websites:

  - https://github.com/jwiegley/use-package

  - https://github.com/theatrus/.emacs.d/blob/master/packages.el

  - https://github.com/agsdot/dotfiles/tree/master/emacs.d

  - https://stackoverflow.com/questions/21064916/auto-install-emacs-packages-with-melpa

  - https://github.com/rejeep/emacs
    - cask initialization, hey he invented cask

  - https://github.com/krobertson/emacs.d

  - https://emacs.stackexchange.com/questions/408/synchronize-packages-between-different-machines/410#410

  - https://github.com/npostavs/emacs.d
    - el get initialization

  - https://github.com/bradleywright/emacs-d
    - https://github.com/bradleywright/emacs-d/blob/master/init-use-package.el

  - https://github.com/jhenahan/emacs.d
    - req package
    - org mode based
      - using pallet / cask
    - has evil setup
      - has evil leader setup

  - http://nathantypanski.com/blog/2014-08-03-a-vim-like-emacs-config.html#use-packageL

  - https://github.com/noaham/dot_emacs/blob/master/init.md

  - https://github.com/kluge/emacs.d/blob/master/init.el

  - https://github.com/samdoshi/.emacs.d/blob/master/emacs-init.org

  - https://github.com/samdoshi/.emacs.d/blob/master/.gitignore
    - good org mode init example

  - https://github.com/dakrone/dakrone-dotfiles/blob/master/.emacs.d/settings.org
    - another good org mode example with use-package

  - http://bnbeckwith.com/bnb-emacs/
    - https://github.com/bnbeckwith/bnb-emacs
    - uses use-package and also a really interesting way of formatting and displaying the emacs.d
      - this? https://github.com/fniessen/org-html-themes

  - https://labo.olivierdelort.net/bricewge/dotfiles-emacs

  - https://github.com/thomasf/dotfiles-thomasf-emacs

  - https://github.com/waymondo/hemacs/

  - https://github.com/jcf/emacs.d

  - https://sriramkswamy.github.io/dotemacs/#orgheadline11
    - https://github.com/sriramkswamy/dotemacs
    - https://www.reddit.com/r/vim/comments/4ms4z0/org_mode_which_plugin_to_use_vimorganizer_or/

  - https://github.com/hlissner/.emacs.d
    - https://www.reddit.com/r/emacs/comments/4n0n8o/what_is_the_best_emacs_mode_line_package/

More notes on setting up Ivy/Swiper/Counsel (a replacament for Ido and Helm)

  - https://cestlaz.github.io/posts/using-emacs-6-swiper/
  - http://oremacs.com/2015/04/16/ivy-mode/
  - http://oremacs.com/2016/01/06/ivy-flx/
    - https://www.reddit.com/r/emacs/comments/3xzas3/help_with_ivycounsel_fuzzy_matching_and_sorting/
  - http://emacsist.com/10865
  - https://www.reddit.com/r/emacs/comments/51lqn9/helm_or_ivy/
  - https://sam217pa.github.io/2016/09/13/from-helm-to-ivy/


el-get setup
  - brew install texinfo
  - brew link texinfo --force
    - https://github.com/dimitri/el-get/issues/2306
