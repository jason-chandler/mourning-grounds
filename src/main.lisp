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

(defparameter event-loop-do t)
(defparameter next-frame-ready t)

(defparameter *dt* 0)

(define-js-callback (ident :type :js-ref)
    ((dt :js-ref))
  dt)

(defparameter render-loop-impl (lambda ()
				 (camera-helper-update-matrix camera-helper)
				 (camera-helper-update camera-helper)
				 (renderer-set-clear-color renderer (js-call "0x000000") 1)
				 (renderer-set-viewport renderer (js-/ window-width 2) 0 (js-/ window-width 2) window-width)))


(define-js-callback (render-loop :type :js-ref)
    ((dt :js-ref))
  (setf *dt* (js-call "Date.now() * 0.0005"))
  (tagbody loop-start
     (setf *dt* (js-- (js-call "Date.now() * 0.0005") *dt*))
     (funcall render-loop-impl)
     (go loop-start)))

(request-animation-frame (js-callback render-loop))
(princ "test")
(js-wait (request-animation-frame (js-callback ident)))


(js-wait "console.log")
