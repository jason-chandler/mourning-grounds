(in-package #:mourning-grounds)


(console-log renderer)

(defparameter ff (lambda () nil))

(define-js-callback (render-loop :type :js-ref)
    ((dt :js-ref)
     (frame :js-ref))
  (setf *dt* dt)
  (setf *dt* (js-- (js-call "Date.now() * 0.0005") *dt*))
  (console-log *dt*)
  (camera-helper-update-matrix camera-helper)
  (camera-helper-update camera-helper)
  (renderer-set-clear-color renderer (js-call "0x0000ff") 1)
  (renderer-set-viewport renderer (js-/ window-width 2) 0 (js-/ window-width 2) window-width)
  (renderer-render renderer scene camera)
  (js-callback render-loop))

(three-position test-mesh)


(console-log

(console-log (far camera))

(renderer-set-animation-loop renderer (js-callback render-loop))
(princ "test")


(console-log scene)
(set-position (three-position camera) -100 20 50)

(defparameter spotlight (js-new (three-class spot-light) (js-call "0xffffff") 60))

(console-log (three-position camera))
(set-position (three-position camera) 5 -11 0)
(set-position (three-position test-mesh) 15 3 -80)

(let ((obj (js-new "Object")))
  (defparameter test-mesh2 (progn
			    (setf (color obj) (js-call "0x00ff00"))
			    (setf (wireframe obj) nil)
			    
			    (js-new (three-class mesh)
				    (js-new (three-class sphere-geometry) 81 16 8)
				    (js-new (three-class mesh-basic-material) obj)))))

(scene-add scene test-mesh2)
(set-position (three-position test-mesh2) 15 6 -79)

(console-log (three-position test-mesh))
(look-at spotlight (three-position test-mesh))

(console-log (three-position camera))

(console-log (three-position test-mesh))
(set-position (three-position camera) 0 0 0)
(defparameter z 0)
(%set! (three-position cam) '(80 90 100))
(console-log (three-position cam))
(console-log (js-new (three-class vector3) 100 100 50))
(defparameter cam (js-new (three-class perspective-camera)))
(%array-push ar (js-call "400"))
(console-log ar)


(defparameter x 0)
(defparameter y 0)
(defparameter z -20)

(defparameter x 23)
(defparameter y -21)

(console-log (three-position camera))


(console-log (instance-request-animation-frame (js-call "window")))

(console-log (js-callback render-loop))

(princ "test")

(request-animation-frame (js-callback render-loop))






