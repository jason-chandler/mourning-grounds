;;; test.lisp
(in-package "EU.TURTLEWARE.WECL/IMPLEMENTATION")

(define-js-function (test-function :js-expr "wecl_test_function" :type :fixnum)
  ((arg :fixnum)))

(define-js-method (test-method :js-expr "example_method" :type :object)
    ((self :object)
     (arg1 :fixnum)
     (arg2 :string)))

(define-js-variable (test-var :js-expr "wecl_test_object"      :type :js-ref))
(define-js-object   (test-obj :js-expr "new wecl_test_class()" :type :js-ref))

(define-js-callback ([print-node] :type :object)
    ((elt :js-ref)
     (nth :fixnum)
     (seq :js-ref))
  (format t "Node ~2d: ~a~%" nth elt))

(defun test-1 ()
  (let ((result (test-function 42)))
    (format t "test_function(42) -> ~s~%" result)))

(defun test-2 ()
  (let ((scripts (query-selector-all [document] "script")))
    (format t "Inline javacall~%")
    (for-each scripts "console.log")
    (format t "Define callback~%")
    (for-each scripts (js-callback [print-node]))
    (format t "Lambda callback~%")
    (for-each scripts (js-callback :object
                          ((elt :string)
                           (nth :fixnum)
                           (seq :js-ref))
                        (format t "Node ~2d: ~a~%" nth elt)))))


;;; Callbacks!

(defun call-lisp-fun (wrapped-fun &rest wrapped-args)
  (let ((fun (unwrap-js-wrapper wrapped-fun))
        (args (mapcar #'unwrap-js-wrapper wrapped-args)))
    (ensure-js-wrapper (apply fun args))))

#+ (or)
(defmacro define-js-callback (name-and-opts arguments &body body)
  (destructuring-bind (name &key js-expr type) name-and-opts
    `(progn
       (defun ,name ,(mapcar #'car arguments) ,@body)

       (let ((fun-ref (ensure-object (function ,name))))
         ())
       (js-eval* "function ~a() {
                    wecl_call(fun_ref 
                    cl_eval(                 ")
       (ensure-object (function ,name)))))

;; (define-js-callback (load-script :type :js-ref)
;;     ((arg :js-ref))
;;   (format t "---~%Script: ~s~%---~%" arg))





#+ (or)
(define-js-script (document :js-expr "document" :type :js-ref)
    ((nodes :js-ref)
     (callb :object))
  "#{nodes}.forEach(#{callb})")

(defvar *x* 0)
(defvar *l* '())
(defun my-callback (&rest args)
  (format *debug-io* "HELLO world!~%")
  (let ((wrapped-args (loop for arg in args
                            collect (js-mptr :js-ref arg))))
    (incf *x*)
    (setf *l* (append wrapped-args *l*))
    (ensure-object :ok)))

(defun my-callback2 (&rest args)
  (format *debug-io* "Yo ~a!~%" args)
  (ensure-object :ok))

(defun test-callback-2 ()
  (call-js-fun :object nil
               "wecl_lcall"
               (ensure-object #'my-callback2)
               "'hello'"))


;;; Slug stream smoke test, relies on libs hence commented out
#+ (or)
(defun smoke-test-slug-stream ()
  (let* ((slug (make-slug-io-stream))
         (istr (two-way-stream-input-stream slug))
         (ostr (two-way-stream-output-stream slug)))
    (write-byte 32 ostr)
    (write-byte 42 ostr)
    (finish-output ostr)
    (flet ((produce ()
             (loop
               (format t "WRITING ~a~%" (write-byte
                                         (mod (get-universal-time) 256)
                                         istr))
               (sleep .03)))
           (consume ()
             (loop
               (format t "READ ~a~%" (read-byte istr))
               (sleep 0.01))))
      (bt:make-thread #'produce)
      (sleep 1)
      (bt:make-thread #'consume))))

(define-js-function (parse-float :js-expr "parseFloat" :type :js-ref)
  ((value :string)))

(define-js-function (date :js-expr "Date" :type :js-ref)
  ())
