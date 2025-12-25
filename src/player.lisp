(in-package #:mourning-grounds)

;; Lock cursor when user clicks document

(define-js-method (request-pointer-lock :js-expr "requestPointerLock" :type :null)
  ((ths :js-ref)))

(add-event-listener %document "mousedown" (lambda-js-callback :null
					      ((ev :js-ref))
					    (request-pointer-lock %doc-body)))

