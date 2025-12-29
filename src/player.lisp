(in-package #:mourning-grounds)

(defparameter world-octree (js-new "Octree"))
(defparameter player-on-ground nil)
(defparameter player-velocity (js-new (three-class vector3)))
(defparameter player-direction (js-new (three-class vector3)))
(defparameter gravity 30)
(defparameter player-collider (js-new "Capsule"
				      (js-new (three-class vector3) 0 (js-call "0.35") 0)
				      (js-new (three-class vector3) 0 1 0)
				      (js-call "0.35")))

(define-js-object (zero :js-expr "0" :type :js-ref))

(define-js-function (math-exp :js-expr "Math.exp" :type :js-ref)
    ((n :js-ref)))

(define-js-method (add-scaled-vector :js-expr "addScaledVector" :type :js-ref)
    ((self :js-ref)
     (vec3 :js-ref)
     (scalar :js-ref)))

(define-js-method (clone :js-expr "clone" :type :js-ref)
    ((self :js-ref)))

(define-js-method (copy :js-expr "copy" :type :js-ref)
    ((self :js-ref)
     (target :js-ref)))

(define-js-method (multiply-scalar :js-expr "multiplyScalar" :type :js-ref)
    ((self :js-ref)
     (scalar :js-ref)))

(define-js-accessor (end :js-expr "end" :type :js-ref)
    ((self :js-ref)
     (new :js-ref)))

(define-js-accessor (start :js-expr "start" :type :js-ref)
    ((self :js-ref)
     (new :js-ref)))


(define-js-method (translate :js-expr "translate" :type :js-ref)
    ((self :js-ref)
     (translation-vector :js-ref)))

(define-js-method (capsule-intersect :js-expr "capsuleIntersect" :type :boolean)
    ((self :js-ref)
     (target :js-ref)))

(defun player-collision-check ()
  (let ((result (capsule-intersect world-octree player-collider)))
    (if result
	(setq player-on-ground (not (eq (math-max (y (normal result)) zero) zero))))))

(define-js-callback (update-player :type :js-ref)
    ((ev :js-ref)
     (dt :js-ref))
  (let ((damping (js-- (math-exp (js-* -4 dt)) 1)))
    (unless player-on-ground
      (setf (y player-velocity) (js-- player-velocity (js-* gravity dt)))
      (setq damping (js-* damping 0.1)))
    (add-scaled-vector player-velocity player-velocity damping)
    (let ((delta-position (multiply-scalar (clone player-velocity) dt)))
      (translate player-collider delta-position)
      (copy (three-position camera) (end player-collider)))))
      
				   
