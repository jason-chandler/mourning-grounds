(in-package #:mourning-grounds)

(define-js-function (console-log :js-expr "console.log" :type :null)
    ((value :js-ref)))

(define-js-function (typeof :js-expr "typeof" :type :js-ref)
    ((value :js-ref)))

(define-js-function (%new :js-expr "new" :type :js-ref)
    ((clss :js-ref)))

(define-js-function (%reflect-construct :js-expr "Reflect.construct" :type :js-ref)
    ((clss :js-ref)
     (args :js-ref)))

(define-js-function (%array :js-expr "Array" :type :js-ref)
    ())

(define-js-method (%array-push :js-expr "push" :type :js-ref)
    ((arr :js-ref)
     (val :js-ref)))


(labels ((guess-type (arg)
	   (typecase arg
	     (number (js-call (format nil "~A" arg)))
	     (null (js-call "null"))
	     (t arg))))
  (defun arg-array (args)
    (let ((arr (%array)))
      (loop :for arg :in args
	    :do (%array-push arr (guess-type arg)))
      arr)))
      
(defun js-new (class-ref &rest constructor-args-obj)
  (if constructor-args-obj
      (%reflect-construct class-ref (arg-array constructor-args-obj))
      (%new class-ref)))

(define-js-getter (prop-set-fn :js-expr "set" :type :js-ref)
    ((ths :js-ref)))

(define-js-method (%set! :js-expr "set(~{~A~^, ~})" :type :js-ref)
    ((ths :js-ref)
     (arg :object)))


(defun set! (prop &rest args)
  (%set! prop (arg-array args)))

  
(define-js-function (js-* :js-expr "((a, b) => a * b)" :type :object)
    ((numerator :object)
     (denominator :object)))

(define-js-function (js-/ :js-expr "((a, b) => a / b)" :type :object)
    ((numerator :object)
     (denominator :object)))

(define-js-function (js-- :js-expr "((a, b) => a - b)" :type :object)
    ((numerator :object)
     (denominator :object)))

(define-js-variable (%document :js-expr "document" :type :symbol))

(define-js-method (%create-element :js-expr "createElement" :type :js-ref)
    ((doc :js-ref)
     (element :string)))

(define-js-object (%doc-body :js-expr "document.body" :type :js-ref))

(define-js-method (%append-child :js-expr "appendChild" :type :js-ref)
  ((body :js-ref)
   (child :js-ref)))

(define-js-method (add-event-listener :js-expr "addEventListener" :type :null)
  ((self :js-ref)
   (name :string)
   (fun :js-ref)))

(labels ((hyphenp (c) (eql c #\-)))
  (defmacro three-class (classname)
    `(js-call (format nil "THREE.~A" ,(remove-if #'hyphenp (string-capitalize classname))))))
