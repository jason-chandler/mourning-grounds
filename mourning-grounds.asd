(defsystem "mourning-grounds"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on (:uiop :alexandria :fset)
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "mourning-grounds/tests"))))

(defsystem "mourning-grounds/tests"
  :author ""
  :license ""
  :depends-on ("mourning-grounds"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for mourning-grounds"
  :perform (test-op (op c) (symbol-call :rove :run c)))
