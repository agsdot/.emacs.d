;;; quse-package.el --- Install and configure packages in one convenient macro. -*- lexical-binding: t; -*-
;; basing off initial commit of quse-package
;; https://github.com/jaccarmac/quse-package/commit/fd7316facec783db0c5c202d45c7350e43fe6821?diff=unified


;;;###autoload
(defmacro egb-use-package2 (el-get-form &rest use-package-forms)
  (let* (

	 (el-get-features (if (and use-package-forms (eq :features (car use-package-forms)))
                           (list (car use-package-forms)
                                 (cadr use-package-forms))))

         (use-package-name el-get-form)

         (stuffel-get-features-present (and use-package-forms (eq :features (car use-package-forms)) ) )

         (use-package-forms (if el-get-features
                                (cddr use-package-forms)
                              use-package-forms))
	 )

    `(progn (el-get-bundle ,el-get-form ,@el-get-features)
            (use-package ,use-package-name
              ,@use-package-forms))))


;;;###autoload
(defmacro egb-use-package5 (el-get-form &rest use-package-forms)
  ;; https://emacs.stackexchange.com/questions/25026/symbols-function-definition-is-void-on-built-in-variables
  ;; You get Symbol's function definition is void because those are variables, not functions.

( setq el-get-features-present2 (and use-package-forms (eq :features (car use-package-forms)) ) )
;;( el-get-features-present (if  use-package-forms (eq :features (car use-package-forms)) ) )

( setq pkgname-present2 (if el-get-features-present2
   	      (eq :pkgname (car (cddr use-package-forms)))
   	    (and use-package-forms (eq :pkgname (car use-package-forms)))))

( setq el-get-features (if el-get-features-present2
                 (list (car use-package-forms) (cadr use-package-forms))))


( setq pkgname         (if pkgname-present2
   	      (if el-get-features-present2
                     (nth 3 use-package-forms)
   		(nth 1 use-package-forms))
   	    el-get-form))

( setq use-package-forms  (if el-get-features-present2
			     (if pkgname-present2
			         (nthcdr 4 use-package-forms)
	                       (nthcdr 2 use-package-forms))
                           (if pkgname-present2
			       (nthcdr 2 use-package-forms)
			       use-package-forms
			   )))


    `(progn (el-get-bundle ,el-get-form ,@el-get-features)
            (use-package ,pkgname
              ,@use-package-forms)))



(provide 'egb-use-package)
