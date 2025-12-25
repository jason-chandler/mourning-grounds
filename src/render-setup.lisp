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
			    (setf (wireframe obj) nil)
			    
			    (js-new (three-class mesh)
				    (js-new (three-class sphere-geometry) 50 16 8)
				    (js-new (three-class mesh-basic-material) obj)))))

(scene-add scene test-mesh)


(defparameter *dt* 0)

(define-js-callback (ident :type :js-ref)
    ((dt :js-ref))
  dt)

(setf (far camera) 2000)
(set-position (three-position camera) 23 -21 -20)
(set-position (three-position test-mesh) 15 3 -80)
(look-at camera (three-position test-mesh))

(renderer-set-animation-loop renderer (lambda-js-callback :js-ref
					  ((dt :js-ref))
					(camera-helper-update-matrix camera-helper)
					(camera-helper-update camera-helper)
					(renderer-set-clear-color renderer (js-call "0xbbbbbb") 1)
					(renderer-set-viewport renderer 0 0 window-width window-height)
					(renderer-render renderer scene camera)))
