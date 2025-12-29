(in-package #:mourning-grounds)

(define-js-object (scene :js-expr "new THREE.Scene()" :type :js-ref))


(defparameter scene (js-new (three-class scene)))

(define-js-variable (window-width :js-expr "window.innerWidth" :type :fixnum))
(define-js-variable (window-height :js-expr "window.innerHeight" :type :fixnum))

(define-js-accessor (aspect :js-expr "aspect" :type :js-ref)
  ((self :js-ref)
   (new :js-ref)))

(define-js-accessor (far :js-expr "far" :type :js-ref)
  ((self :js-ref)
   (new :fixnum)))

(define-js-accessor (three-position :js-expr "position" :type :js-ref)
  ((self :js-ref)
   (new :js-ref)))

(define-js-method (set-position :js-expr "set" :type :js-ref)
  ((pos :js-ref)
   (x :object)
   (y :object)
   (z :object)))

(define-js-accessor (normal :js-expr "normal" :type :js-ref)
    ((self :js-ref)
     (new :js-ref)))

(define-js-accessor (x :js-expr "x" :type :js-ref)
  ((self :js-ref)
   (new :js-ref)))

(define-js-accessor (y :js-expr "y" :type :js-ref)
  ((self :js-ref)
   (new :js-ref)))

(define-js-accessor (z :js-expr "z" :type :js-ref)
  ((self :js-ref)
   (new :js-ref)))

(define-js-accessor (rotation :js-expr "rotation" :type :js-ref)
  ((self :js-ref)
   (new :js-ref)))

(define-js-accessor (order :js-expr "order" :type :string)
  ((self :js-ref)
   (new :string)))

(define-js-method (look-at :js-expr "lookAt" :type :js-ref)
  ((self :js-ref)
   (target :js-ref)))

(let ((aspect (js-/ window-width window-height))
      (half (js-call "0.5")))
  (defparameter camera (js-new (three-class perspective-camera) 70 aspect 0.1 1000)))

(setf (order (rotation camera)) "YXZ")

(defparameter camera-helper (js-new (three-class camera-helper) camera))

(define-js-method (camera-helper-update :js-expr "update" :type :null)
    ((cam-helper :js-ref)))

(define-js-method (camera-helper-update-matrix :js-expr "updateMatrix" :type :null)
    ((cam-helper :js-ref)))

(define-js-method (scene-add :js-expr "add" :type :null)
    ((scn :js-ref)
     (new-scn-member :js-ref)))

(scene-add scene camera-helper)

(defparameter container (%create-element %document "div"))
(%append-child %doc-body container)

(defparameter renderer (js-new (three-class web-g-l-renderer) (js-call "{ antialias: true }")))

(define-js-method (renderer-set-pixel-ratio :js-expr "setPixelRatio" :type :null)
    ((renderer :js-ref)
     (ratio :js-ref)))

(define-js-method (renderer-set-size :js-expr "setSize" :type :null)
    ((renderer :js-ref)
     (width :js-ref)
     (height :js-ref)))

(define-js-accessor (instance-request-animation-frame :js-expr "requestAnimationFrame" :type :js-ref)
    ((renderer :js-ref)
     (anim-fn :js-ref)))

(define-js-getter (renderer-dom-element :js-expr "domElement" :type :js-ref)
    ((renderer :js-ref)))

(define-js-method (renderer-set-animation-loop :js-expr "setAnimationLoop" :type :null)
    ((renderer :js-ref)
     (callback :js-ref)))

(renderer-set-pixel-ratio renderer (js-call "window.devicePixelRatio"))
(renderer-set-size renderer window-width window-height)

(%append-child container (renderer-dom-element renderer))

(define-js-method (renderer-render :js-expr "render" :type :null)
    ((renderer :js-ref)
     (scene :js-ref)
     (cam :js-ref)))

(define-js-function (request-animation-frame :js-expr "window.requestAnimationFrame" :type :js-ref)
    ((callback :js-ref)))

(scene-add scene (js-new (three-class ambient-light) (js-call "0xffffff")))
				       
