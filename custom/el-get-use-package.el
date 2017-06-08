;; basing off initial commit of quse-package
;; https://github.com/jaccarmac/quse-package/commit/fd7316facec783db0c5c202d45c7350e43fe6821?diff=unified

(defmacro el-get-use-package (el-get-form &rest use-package-forms)
  "Download a package with el-get-bundle and initialize it with use-package.
   el-get-form should be a name compatible with el-get recipes.
   use-package-form should be whatever comes after the package name in a use-package call."
  (let ((use-package-name (if (listp el-get-form)
                              (car el-get-form)
                            el-get-form)))
    `(progn (el-get-bundle ,el-get-form)
            (use-package ,use-package-name
              ,@use-package-forms))))

(provide 'el-get-use-package)
