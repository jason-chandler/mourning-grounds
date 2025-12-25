;;;
;;; FRIG - Fragile Remote Interaction Garret
;;; This file implements non-portable parts for slug.lisp.
;;; 
;;; This is a test for LIME using any Common Lisp (not necessarily WECL).

;;; I don't like how this library is structured, but let's use it for the sake
;;; of simplicity - it is only a test after all. If push comes to the shove we
;;; can always fork it and rewrite icky parts.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("websocket-driver-client" "bordeaux-threads")))

(defpackage "EU.TURTLEWARE.WECL/FRIG"
  (:use "CL" "EU.TURTLEWARE.WECL/SLUG"))
(in-package "EU.TURTLEWARE.WECL/FRIG")

(defun add-event-listener (client event handler)
  (websocket-driver:on event client handler))

(defclass frig-client ()
  ((sock :initarg :ws :reader ws)
   (conn :initarg :conn :accessor ws-connection)))

(defmethod ws-send ((client frig-client) message)
  (websocket-driver:send (ws client) message))

(defmethod ws-disconnect ((client frig-client))
  (websocket-driver:close-connection (ws client)))

(defun frig-connect (host port)
  (let* ((ws (websocket-driver:make-client (format nil "ws://~a:~a" host port)))
         (client (make-instance 'frig-client :ws ws)))
    (flet ((ws-open ()
             (ws-open client nil))
           (ws-recv (msg)
             (ws-recv client msg))
           (ws-fail (msg)
             (ws-fail client msg) )
           (ws-kill (&key code reason)
             (ws-kill client (cons code reason))))
      (add-event-listener ws :open    #'ws-open)
      (add-event-listener ws :message #'ws-recv)
      (add-event-listener ws :error   #'ws-fail)
      (add-event-listener ws :close   #'ws-kill))
    (websocket-driver:start-connection ws)
    client))


;;; Adding blocking to a slug stream.
;;; 
;;; We assume that there is a single producent and a single consumer. That means
;;; in particular that a) we may reuse the condition variable, b) no concurrent
;;; access between corresponding ensure/notify pairs.
;;; 
;;; Philosoraptor: is space the resource or is it input?

;;; FIXME swank already has locks, all we need to contribute are condition
;;; variables.
(defun make-cvar () (bt:make-condition-variable))
(defun make-lock () (bt:make-lock))
(defmacro wait-cv (test cv lock)
  `(loop with cvar = ,cv
         with lock = ,lock do
           (bt:with-lock-held (lock)
             (if ,test
                 (loop-finish)
                 (bt:condition-wait cvar lock)))))
(defun notify-cv (cv)
  (bt:condition-notify cv))

(defclass slug-safe-stream (slug-ring-stream)
  ((cvar :initform (make-cvar))
   (lock :initform (make-lock))))

(defmethod slug-ensure-input-available-p ((self slug-safe-stream))
  (unless (slug-input-available-p self)
    (with-slots (cvar lock) self
      (wait-cv (slug-input-available-p self) cvar lock))))

(defmethod slug-notify-input-available-p ((self slug-safe-stream))
  (with-slots (cvar lock) self
    (notify-cv cvar)))

(defmethod slug-ensure-space-available-p ((self slug-safe-stream))
  (unless (slug-space-available-p self)
    (with-slots (cvar lock) self
      (wait-cv (slug-space-available-p self) cvar lock))))

(defmethod slug-notify-space-available-p ((self slug-safe-stream))
  (with-slots (cvar lock) self
    (notify-cv cvar)))

;;; By starting the async handler in a new thread we ensure that WS-RECV is not
;;; blocked when FRIG waits for the next event. Recursive calls are also ordered
;;; because there is a single blocking agent that handles requests.
(defmethod serve-requests ((client frig-client) connection)
  (ecase (swank::connection.communication-style connection)
    (:spawn (swank::serve-requests connection))
    (:async (bt:make-thread
             (lambda ()
               ;; slug-safe-stream blocks when there is no new input, so we
               ;; don't need to check whether the request is complete or not.
               (let ((stream (swank::connection.socket-io connection)))
                 (unwind-protect
                      (loop while (open-stream-p stream) do
                        (handler-bind ((warning (lambda (c) (muffle-warning c))))
                          (async-serve-request connection stream)))
                   (close stream))))))))


(defparameter *frig-style* :async)

(defmethod ws-open ((client frig-client) message)
  (declare (ignore message))
  (let* ((stream (make-two-way-stream
                  (make-instance 'slug-safe-stream :socket client)
                  (make-instance 'slug-send-stream :socket client)))
         (connection (swank::make-connection client stream *frig-style*)))
    (setf (ws-connection client) connection)
    (serve-requests client connection)))

(defun test-frig ()
  (frig-connect "localhost" 8889))
