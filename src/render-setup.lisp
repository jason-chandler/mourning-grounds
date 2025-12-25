(in-package #:mourning-grounds)

(define-js-accessor (color :js-expr "color" :type :js-ref)
    ((self :js-ref)
     (new :js-ref)))

(define-js-accessor (wireframe :js-expr "wireframe" :type :js-ref)
    ((self :js-ref)
     (new :boolean)))

(define-js-method (renderer-set-clear-color :js-expr "setClearColor" :type :js-ref)
  ((self :js-ref)
   (color :js-ref)
   (one :fixnum)))

(define-js-method (renderer-set-viewport :js-expr "setViewport" :type :js-ref)
  ((self :js-ref)
   (half-width1 :js-ref)
   (zero :fixnum)
   (half-width2 :js-ref)
   (height :js-ref)))


(let ((obj (js-new "Object")))
  (defparameter test-mesh (progn
			    (setf (color obj) (js-call "0x0000ff"))
			    (setf (wireframe obj) t)
			    
			    (js-new (three-class mesh)
				    (js-new (three-class sphere-geometry) 50 16 8)
				    (js-new (three-class mesh-basic-material) obj)))))

(scene-add scene test-mesh)

(define-js-accessor (far :js-expr "far" :type :js-ref)
  ((self :js-ref)
   (new :fixnum)))

(define-js-accessor (three-position :js-expr "position" :type :js-ref)
  ((self :js-ref)
   (new :js-ref)))

(define-js-method (look-at :js-expr "lookAt" :type :js-ref)
  ((self :js-ref)
   (target :js-ref)))

(defparameter *dt* 0)

(define-js-callback (ident :type :js-ref)
    ((dt :js-ref))
  dt)
