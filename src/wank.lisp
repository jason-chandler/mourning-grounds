;;;
;;; WANK - Web Adapter for SWANK (like SWANK, but not Superior)
;;; This file implements non-portable parts for slug.lisp.
;;; 
;;; This is an in-browser client for LIME using WECL.

(in-package "EU.TURTLEWARE.WECL/IMPLEMENTATION")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (do-external-symbols (s "EU.TURTLEWARE.WECL/SLUG")
    (import s)))

(define-js-script (make-ws :js-expr "new WebSocket('ws://~a:~a')" :type :js-ref)
  ((host :symbol)
   (port :fixnum)))

(define-js-method (js-ws-send :js-expr "send" :type :null)
  ((socket :js-ref)
   (string :string)))

(define-js-method (js-ws-close :js-expr "close" :type :null)
  ((socket :js-ref)))

(define-js-method (add-event-listener :js-expr "addEventListener" :type :null)
  ((object :js-ref)
   (event :string)
   (callback :js-ref)))

(define-js-getter (ws-msg :js-expr "data" :type :string)
    ((self :js-ref)))

(defclass wank-client ()
  ((sock :initarg :ws :reader ws)
   (conn :initarg :conn :accessor ws-connection
         :initform nil)))

;;; FIXME map binary data with UHEAP8
(defmethod ws-send ((client wank-client) message)
  (unless (stringp message)
    (setf message (swank::utf8-to-string message)))
  (js-ws-send (ws client) message))

(defmethod ws-disconnect ((client wank-client))
  (js-ws-close (ws client)))

;;; Swank bridge (wip)
(defun wank-connect (host port)
  (let* ((ws (make-ws host port))
         (client (make-instance 'wank-client :ws ws)))
    (add-event-listener ws "open"
                        (js-callback :null ((event :js-ref))
                          (ws-open client nil)))
    (add-event-listener ws "message"
                        (js-callback :null ((event :js-ref))
                          (ws-recv client (ws-msg event))))
    (add-event-listener ws "close"
                        (js-callback :null ((event :js-ref))
                          (ws-kill client "WS connection closed..")))
    (add-event-listener ws "onerror"
                        (js-callback :null ((event :js-ref))
                          (ws-fail client "WS connection error!")))
    
    client))


;;; Adding blocking to a slug stream.
;;; 
;;; This implementation is tailored for JS event loop. When there is no input
;;; available, we yield.

;;; We retain the "shape" of the implementation so that both backends look
;;; similar (for readibility).
(progn
  (defun make-cvar () nil)
  (defun make-lock () nil)
  (defmacro wait-cv (test cv lock)
    (declare (ignore cv lock))
    `(loop until (progn ,test) do
      (js-yield)))
  (defun notify-cv (cv)
    (declare (ignore cv))))

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

(defmethod serve-requests ((client wank-client) connection)
  (ecase (swank::connection.communication-style connection)
    (:async
     ;; slug-safe-stream yields when there is no new input, so we don't need to
     ;; check whether the request is complete or not. Moreover, unlike FRIG, we
     ;; don't need to have this loop in a separate thread (thanks to yielding).
     (let ((stream (swank::connection.socket-io connection)))
       (unwind-protect
            (loop while (open-stream-p stream) do
              (handler-bind ((warning (lambda (c) (muffle-warning c))))
                (async-serve-request connection stream)))
         (close stream))))))


(defmethod ws-open ((client wank-client) message)
  (declare (ignore message))
  (let* ((stream (make-two-way-stream
                  (make-instance 'slug-safe-stream :socket client)
                  (make-instance 'slug-send-stream :socket client)))
         (connection (swank::make-connection client stream :async)))
    (setf (ws-connection client) connection)
    (serve-requests client connection)))

;;; We need to ensure that the message is complete, because when there is no new
;;; input while we read slug-ring-stream will signal an error. That will happen
;;; also during recursive calls because we block the thread, so the websocket
;;; can't actually receive new data (despite sending a request).
;;; 
;;; This method is not necessary anymore, because now we have async stream. With
;;; this method we could have worked only with top-level requests.
#+ (or)
(defmethod ws-recv :after ((client wank-client) message)
  (let* ((connection (ws-connection client))
         (style (swank::connection.communication-style connection))
         (stream (swank::connection.socket-io connection))
         (in-str (two-way-stream-input-stream stream)))
    (when (eql style :async)
      (loop while (complete-message-p in-str) do
        (handler-bind ((warning (lambda (c) (muffle-warning c))))
          (async-serve-request connection stream))))))

(defun test-wank ()
  (wank-connect "localhost" 8889))
