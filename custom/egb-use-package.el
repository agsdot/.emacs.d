;;; quse-package.el --- Install and configure packages in one convenient macro. -*- lexical-binding: t; -*-
;; basing off initial commit of quse-package
;; https://github.com/jaccarmac/quse-package/commit/fd7316facec783db0c5c202d45c7350e43fe6821?diff=unified

;;;###autoload
(defmacro egb-use-package (el-get-form &rest use-package-forms)
  "Download a package with el-get-bundle and initialize it with use-package.
   el-get-form should be a name compatible with el-get recipes.
   use-package-form should be whatever comes after the package name in a use-package call."
  (let ((use-package-name (if (listp el-get-form)
                              (car el-get-form)
                            el-get-form)))
    `(progn (el-get-bundle ,el-get-form)
            (use-package ,use-package-name
              ,@use-package-forms))))


;;;###autoload
(defmacro egb-use-package2 (el-get-form &rest use-package-forms)
  (let* ((el-get-features (if (and use-package-forms
                                (eq :features (car use-package-forms)))
                           (list (car use-package-forms)
                                 (cadr use-package-forms))))
         (use-package-name el-get-form)
         (use-package-forms (if el-get-features
                                (cddr use-package-forms)
                              use-package-forms)))
    `(progn (el-get-bundle ,el-get-form ,@el-get-features)
            (use-package ,use-package-name
              ,@use-package-forms))))

;; next step, provide way for the el-get-bundle name and use-package to be named differently

(provide 'egb-use-package)
