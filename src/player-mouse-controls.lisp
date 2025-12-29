(in-package #:mourning-grounds)

;; Lock cursor when user clicks document

(define-js-getter (fetch-canvas :js-expr "children[1].children[0]" :type :js-ref)
    ((docbody :js-ref)))

(define-js-method (request-pointer-lock :js-expr "requestPointerLock" :type :null)
  ((ths :js-ref)))

(add-event-listener (fetch-canvas %doc-body) "mousedown" (lambda-js-callback :null
							     ((ev :js-ref))
							   (request-pointer-lock %doc-body)))

(define-js-getter (pointer-lock-element :js-expr "pointerLockElement" :type :js-ref)
    ((ths :js-ref)))

(define-js-accessor (movement-x :js-expr "movementX" :type :js-ref)
  ((self :js-ref)
   (new :js-ref)))

(define-js-accessor (movement-y :js-expr "movementY" :type :js-ref)
  ((self :js-ref)
   (new :js-ref)))

(add-event-listener %doc-body "mousemove" (lambda-js-callback :null
					      ((ev :js-ref))
					    (when (equal (pointer-lock-element %document) %doc-body)
					      (setf (y (rotation camera)) (js-- (y (rotation camera)) (js-/ (movement-x ev) (js-call "500"))))
					      (setf (x (rotation camera)) (js-- (x (rotation camera)) (js-/ (movement-y ev) (js-call "500")))))))
