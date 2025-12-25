(in-package :mourning-grounds)

(defmacro import-implementation-macro (sym)
  (let ((found-sym (gensym)))
    `(let ((,found-sym ',(intern (string-upcase sym) (find-package "EU.TURTLEWARE.WECL/IMPLEMENTATION"))))
       (import `(macro-function ,,found-sym)))))

(progn 
  (import-implementation-macro "define-js-accessor")
  (import-implementation-macro "define-js-callback")
  (import-implementation-macro "define-js-constant")
  (import-implementation-macro "define-js-event")
  (import-implementation-macro "define-js-function")
  (import-implementation-macro "define-js-getter")
  (import-implementation-macro "define-js-method")
  (import-implementation-macro "define-js-object")
  (import-implementation-macro "define-js-setter")
  (import-implementation-macro "define-js-variable")
  (import-implementation-macro "js-callback"))

