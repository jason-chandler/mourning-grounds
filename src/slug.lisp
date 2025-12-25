
;;;
;;; SLUG - SLIME LIME UGLY GLUE
;;; 
;;; SWANK adapter to use with websockets and act as a client.

#+quicklisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("swank")))

#-quicklisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "SWANK")
    (load "slime/swank-loader.lisp")
    (eval (read-from-string "(swank-loader:init)"))))

(defpackage "EU.TURTLEWARE.WECL/SLUG"
  (:use "CL")
  (:export "WS-OPEN" "WS-KILL" "WS-FAIL" "WS-RECV" "WS-SEND"
           "WS-CONNECTION" "WS-DISCONNECT"
           "SLUG-RING-STREAM" "SLUG-SEND-STREAM"
           "SLUG-INPUT-AVAILABLE-P"
           "SLUG-ENSURE-INPUT-AVAILABLE-P" "SLUG-NOTIFY-INPUT-AVAILABLE-P"
           "SLUG-SPACE-AVAILABLE-P"
           "SLUG-ENSURE-SPACE-AVAILABLE-P" "SLUG-NOTIFY-SPACE-AVAILABLE-P"
           ;; Entry point for serving top-level requests (!).
           "SERVE-REQUESTS" "ASYNC-SERVE-REQUEST" "COMPLETE-MESSAGE-P" "SLUG-DBG"))
(in-package "EU.TURTLEWARE.WECL/SLUG")

;;; These functions are defined as generic function so that we can specialize
;;; them to different websocket implementations. Most notably sending data.
(defgeneric ws-open (client message))
(defgeneric ws-kill (client message))
(defgeneric ws-fail (client message))
(defgeneric ws-recv (client message))

;;; These functions must be specialized.
(defgeneric ws-send (client message))
(defgeneric ws-disconnect (client))
(defgeneric ws-connection (client))


(defvar *ring-buffer-size* (* 1024 1024))

;;; SWANK expects an octety stream for I/O.
(defclass slug-stream-mixin ()
  ((sock :initarg :socket :reader slug-stream-socket)))

(defmethod slug-stream-socket ((stream two-way-stream))
  (slug-stream-socket (two-way-stream-input-stream stream)))

(defmethod #+ecl gray:stream-element-type
           #-ecl stream-element-type
    ((self slug-stream-mixin))
  '(unsigned-byte 8))

;;; In a ring buffer we waste one element to not confuse empty and full states.
(defclass slug-ring-stream (slug-stream-mixin
                            swank/gray:fundamental-binary-input-stream
                            swank/gray:fundamental-binary-output-stream)
  ((sock :initarg :socket :reader slug-stream-socket)
   (ibuf :initform (make-array *ring-buffer-size* :element-type '(unsigned-byte 8)))
   (scan :initform 0)
   (edit :initform 0)))

(defun slug-input-available-p (self)
  (with-slots (ibuf scan edit) self
    (/= scan edit)))

(defun slug-space-available-p (self)
  (with-slots (ibuf scan edit) self
    (/= (mod (1+ edit) (length ibuf)) scan)))

(defun slug-peek-input-length (self)
  (with-slots (ibuf scan edit) self
    (if (>= edit scan)
        (- edit scan)
        (+ (- (length ibuf) scan) edit))))

(defun slug-peek-input-buffer (self bytes)
  (let ((nitems (slug-peek-input-length self)))
    (when (< nitems bytes)
      (return-from slug-peek-input-buffer nil))
    (with-slots (ibuf scan edit) self
      (loop with length = (length ibuf)
            repeat bytes
            for i = scan then (mod (1+ i) length)
            collect (aref ibuf i) into result
            finally (return (values (coerce result 'vector) nitems))))))

;;; XXX why oh why LISTEN is specified only for character streams? Nobody
;;; knows.. Let's pretend that this knowledge is not in our possession and that
;;; the underlying implementation does the right thing.
(defmethod swank/gray:stream-listen ((self slug-ring-stream))
  (slug-input-available-p self))

;;; These functions may i.e yield, poll, wait on cv, error.
(defgeneric slug-ensure-input-available-p (self)
  (:method ((self slug-ring-stream))
    (unless (slug-input-available-p self)
      (error "No input available in the ring buffer."))))

(defgeneric slug-ensure-space-available-p (self)
  (:method ((self slug-ring-stream))
    (unless (slug-space-available-p self)
      (error "No space available in the ring buffer."))))

;;; These functions may i.e resume, do nothing, notify cv.
(defgeneric slug-notify-input-available-p (self)
  (:method ((self slug-ring-stream))))

(defgeneric slug-notify-space-available-p (self)
  (:method ((self slug-ring-stream))))

(defmethod swank/gray:stream-read-byte ((self slug-ring-stream))
  (with-slots (ibuf scan edit) self
    (slug-ensure-input-available-p self)
    (prog1 (aref ibuf scan)
      (setf scan (mod (1+ scan) (length ibuf)))
      (slug-notify-space-available-p self))))

(defmethod swank/gray:stream-write-byte ((self slug-ring-stream) byte)
  (with-slots (ibuf scan edit) self
    (slug-ensure-space-available-p self)
    (prog1 (setf (aref ibuf edit) byte)
      (setf edit (mod (1+ edit) (length ibuf)))
      (slug-notify-input-available-p self))))

(defmethod swank/gray:close ((self slug-ring-stream) &key abort)
  (ws-disconnect (slug-stream-socket self)))

(defclass slug-send-stream (slug-stream-mixin
                            swank/gray:fundamental-binary-output-stream)
  ((sock :initarg :socket :reader slug-stream-socket)
   (obuf :initform (make-array 1024 :element-type '(unsigned-byte 8)
                                    :fill-pointer 0 :adjustable t))))

(defmethod swank/gray:stream-write-byte ((self slug-send-stream) byte)
  (with-slots (obuf) self
    (vector-push-extend byte obuf)))

(defmethod swank/gray:stream-finish-output ((self slug-send-stream))
  (with-slots (sock obuf) self
    (ws-send sock obuf)
    (setf (fill-pointer obuf) 0)))

(defmethod swank/gray:stream-force-output ((self slug-send-stream))
  (with-slots (sock obuf) self
    (ws-send sock obuf)
    (setf (fill-pointer obuf) 0)))

(defun make-slug-io-stream (socket)
  (make-two-way-stream (make-instance 'slug-ring-stream :socket socket)
                       (make-instance 'slug-send-stream :socket socket)))


;;; SWANK flow in a basic case could be simplified as following[1]:
;;;
;;; (setup-server)
;;;   (accept-connections #<socket>)
;;;     #<stream> <- (accept-connection)
;;;     (authenticate-client #<stream>)
;;;     #<connection> <- (make-connection #<stream>)
;;;     (serve-requests #<connection>)
;;; 
;;; SERVE-REQUESTS comes in a few flavours depending on the connection type, but
;;; the gist of it is that the reader decodes requests sent through the stream
;;; and encodes answers that are sent back. The dynamic context matters - we
;;; can't always handle requests from the top-level because they can be nested.

(defun zipit (obj &optional (start 0) (end 72))
  (let* ((msg (typecase obj
                (string obj)
                (vector (format nil "~a" (swank::utf8-to-string obj)))
                (otherwise (format nil "??~a??" obj))))
         (len (length msg)))
    (subseq msg (min start len) (min end len))))

(defmacro when-let ((var form) &body body)
  `(let ((,var ,form))
     (when ,var ,@body)))

;;; We use this stream for debug to avoid rebinding by SWANK.
(defvar *dbg-io* *standard-output*)
(defun slug-dbg (fmt &rest args)
  ;(apply #'format *dbg-io* fmt args)
  )

;;; c.f REPL-INPUT-STREAM-READ
(defun async-serve-request (connection stream)
  ;; XXX something in websocket-driver-client binds this variable to nil.
  (setf *read-eval* t)  
  (slug-dbg "ASYNC-SERVE-REQUEST~%")
  (with-simple-restart (process-input "Continue reading input.")
    (let ((swank:*sldb-quit-restart* (find-restart 'process-input)))
      (swank::with-io-redirection (connection)
        (swank::with-connection (connection)
          ;; FIXME here we ignore slime interrupts etc.
          (slug-dbg "Waiting for next request...~%")
          (let ((event (swank::decode-message stream)))
            ;; A Slime request from Emacs is pending; redirect IO to the REPL buffer.
            (slug-dbg "Serving a new request ~s...~%" (zipit event 0 60))
            (swank::dcase event
              ((:emacs-rex form buffer-package thread id)
               (declare (ignore thread))
               (swank::eval-for-emacs form buffer-package id))
              ((:emacs-channel-send channel (selector &rest args))
               (swank::channel-send channel selector args))
              (t
               (slug-dbg ":ASYNC does not handle ~s.~%" (car event))))
            (slug-dbg "Request served.~%")))))))

(defgeneric serve-requests (client connection)
  (:method (client connection)
    (ecase (swank::connection.communication-style connection)
      (:spawn (swank::serve-requests conn))
      (:async nil))))


;;; Default WS callback methods

(defmethod ws-fail (client message)
  (slug-dbg "Failure ~a ~a~%" client message))

(defmethod ws-recv (client message)
  (let* ((connection (ws-connection client))
         (socket-io (two-way-stream-input-stream
                     (swank::connection.socket-io connection))))
    (if (stringp message)
        (write-sequence (swank::string-to-utf8 message) socket-io)
        (write-sequence message socket-io))))

;;; It is important for ASYNC-SERVE-REQUEST to be invoked only when a full
;;; request is available, because the default implementation of the ring buffer
;;; will signal an error when there is no more data.
(defun complete-message-p (stream)
  (multiple-value-bind (header length)
      (slug-peek-input-buffer stream 6)
    (let* ((header-string (map 'string #'code-char header))
           (message-length (parse-integer header-string :radix 16
                                                        :junk-allowed t)))
      (and message-length (<= message-length length)))))

(defmethod ws-recv :after (client message)
  (slug-dbg "RECV: ~s~%" (zipit message 6 72)))

(defmethod ws-send :after (client message)
  (slug-dbg "SEND: ~s~%" (zipit message 6 72)))

(defmethod ws-kill (client message)
  (declare (ignore message))
  (slug-dbg "KILL: ~s~%" (zipit message))
  (when-let (connection (ws-connection client))
    (swank::close-connection connection nil (swank::safe-backtrace))))
