;;; bsd-2-clause, (c) 2022, Daniel Kochmański

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("hunchentoot" "hunchensocket" "log4cl" "swank")))

(defpackage "WEBSERVER"
  (:use "CL")
  (:locaL-nicknames ("HT" "HUNCHENTOOT")
                    ("WS" "HUNCHENSOCKET")))
(in-package "WEBSERVER")


;;; Start http
(defvar *directory* (uiop/os:getcwd))

(defvar *ht-acceptor*
  (make-instance 'hunchentoot:easy-acceptor :port 8888))

(defun make-dispatch-directory (prefix directory)
  (flet ((handler ()
           (hunchentoot:no-cache)
           (setf (hunchentoot:header-out "Access-Control-Allow-Origin") "*")
           (let ((path (hunchentoot:request-pathname hunchentoot:*request* prefix)))
             (when (null path)
               (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+)
               (hunchentoot:abort-request-handler))
             (hunchentoot:handle-static-file (merge-pathnames path directory)))))
    (hunchentoot:create-prefix-dispatcher prefix #'handler)))

(print `(serving ,*directory*))
(push (make-dispatch-directory "/" *directory*) hunchentoot:*dispatch-table*)
(hunchentoot:start *ht-acceptor*)


;;; Start ws
(defclass swank-proxy (hunchensocket:websocket-resource)
  ()
  (:default-initargs :client-class 'swank-client))

(defclass swank-client (hunchensocket:websocket-client)
  ())

;;; Intern these keywords as a workaround for chunga, see [1] and [2]:
;;; [1] https://github.com/edicl/chunga/issues/14
;;; [2] https://github.com/edicl/chunga/pull/15
(progn :dnt :sec-gpc :sec-fetch-dest :sec-fetch-mode :sec-fetch-site)

(defvar *wank* (make-instance 'swank-proxy))

(defvar *ws-acceptor*
  (make-instance 'hunchensocket:websocket-acceptor :port 8889))

(push (constantly *wank*) hunchensocket:*websocket-dispatch-table*)

(defmethod hunchensocket:client-connected ((resource swank-proxy) client)
  (log:info "~s: connected ~s" resource client))

(defmethod hunchensocket:client-disconnected ((resource swank-proxy) client)
  (log:info "~s: diconnected ~s" resource client))

(defmethod hunchensocket:text-message-received ((resource swank-proxy) client text)
  (log:info "~s: text msg from ~s: ~s" resource client text))

(defmethod hunchensocket:binary-message-received ((resource swank-proxy) client bin)
  (log:info "~s: binary msg from ~s: ~s" resource client bin))

(defun broadcast (message &rest args)
  (loop for peer in (hunchensocket:clients *wank*)
        do (hunchensocket:send-text-message peer (apply #'format nil message args))))

;;; WINE would go here.

;; (hunchentoot:start *ws-acceptor*)



(swank:create-server :port 4444)
