(in-package #:mourning-grounds)

(defparameter render-loop-impl (lambda ()
				 (camera-helper-update-matrix camera-helper)
				 (camera-helper-update camera-helper)
				 (renderer-set-clear-color renderer (js-call "0x000000") 1)
				 (renderer-set-viewport renderer (js-/ window-width 2) 0 (js-/ window-width 2) window-width)
				 (renderer-render renderer scene camera)))

(define-js-method (bind :js-expr "__proto__.bind" :type :js-ref)
  ((self :js-ref)
   (ths :js-ref)))


(define-js-callback (render-loop :type :js-ref)
    ((dt :js-ref)
     (frame :js-ref))
  (setf *dt* dt)
  (setf *dt* (js-- (js-call "Date.now() * 0.0005") *dt*))
  (funcall render-loop-impl))

(renderer-set-animation-loop renderer (js-callback render-loop))

(setf (instance-request-animation-frame scene) (instance-request-animation-frame (js-call "window")))
(console-log (instance-request-animation-frame (js-call "window")))

(bind (js-callback render-loop) renderer)

(console-log (js-callback render-loop))

(princ "test")

(request-animation-frame (js-callback render-loop))






