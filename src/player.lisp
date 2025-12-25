(in-package #:mourning-grounds)

;; Lock cursor when user clicks document

(define-js-getter (fetch-canvas :js-expr "children[1].children[0]" :type :js-ref)
    ((docbody :js-ref)))

(define-js-method (request-pointer-lock :js-expr "requestPointerLock" :type :null)
  ((ths :js-ref)))

(add-event-listener (fetch-canvas %doc-body) "mousedown" (lambda-js-callback :null
					      ((ev :js-ref))
					  (request-pointer-lock %doc-body)))

