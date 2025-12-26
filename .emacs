(require 'slime-autoloads)
(setq slime-contribs '(slime-fancy))


;;; lime.el --- Lisp Interaction Mode for Emacs -*-lexical-binding:t-*-
;;; 
;;; This program extends SLIME with an ability to listen for lisp connections.
;;; The flow is reversed - normally SLIME is a client and SWANK is a server.
;;; 
;;; Aka "my first Emacs Lisp program" - don't let silly comments bother you.

(require 'websocket)

;;; SLIME operates on Emacs processes. For example SLIME-NET-CONNECT returns a
;;; network process, while SLIME-START-LISP returns a process associated with
;;; the inferior-lisp. To communicate with the process we use functions:
;;;
;;; - filter   :: called when the process has new output
;;; - sentinel :: called when the process changes status
;;; - PROCESS-SEND-* :: user calls these functions to send data 
;;; 
;;; Process may have an associated buffer. We can associate local variables with
;;; a buffer using MAKE-LOCAL-VARIABLE. We can't pass WEBSOCKET-CONN to SLIME as
;;; the process, because the latter won't format messages according to WS proto.

(defvar *lime-server* nil
  "The LIME server.")

(cl-defun lime-zipit (obj &optional (start 0) (end 72))
  (let* ((msg (if (stringp obj)
                  obj
                (slime-prin1-to-string obj)))
         (len (length msg)))
    (substring msg (min start len) (min end len))))

(cl-defun lime-message (&rest args)
  (with-current-buffer (process-buffer *lime-server*)
    (goto-char (point-max))
    (dolist (arg args)
      (insert (lime-zipit arg)))
    (insert "\n")
    (goto-char (point-max))))

;;; SLIME/SWANK can't decide whether they work with binary data or strings.
;;; SLIME sends a string (with all glory of encoding issues), while SWANK parses
;;; binary data that is then coerced back to string. In any case both methods
;;; work in practice.
(cl-defun lime-send-text (client data)
  (websocket-send-text client data))

(cl-defun lime-send-data (client data)
  (websocket-send
   client
   (make-websocket-frame :opcode 'binary
                         :payload data
                         :completep t)))

(cl-defun lime-client-process (client)
  (websocket-conn client))

(cl-defun lime-process-client (process)
  (process-get process :websocket))

(cl-defun lime-process-send-string (process string)
  (let ((client (lime-process-client process)))
    (websocket-send-text client string)))

;;; c.f slime-net-connect
(cl-defun lime-add-client (client)
  (lime-message "LIME connecting a new client")
  (let* ((process (websocket-conn client))
         (buffer (generate-new-buffer "*lime-connection*")))
    (set-process-buffer process buffer)
    (process-put process 'slime-net-send-function #'lime-process-send-string)
    (push process slime-net-processes)
    (slime-setup-connection process)
    client))

;;; When SLIME kills the process, then it invokes LIME-DISCONNECT hook.
;;; When SWANK kills the process, then it invokes LIME-DEL-CLIENT hook.
(cl-defun lime-del-client (client)
  (when-let ((process (lime-client-process client)))
    (lime-message "LIME client disconnected")
    (slime-net-sentinel process "closed by peer")))

(cl-defun lime-disconnect (process)
  (when-let ((client (lime-process-client process)))
    (lime-message "LIME disconnecting client")
    (websocket-close client)))

(cl-defun lime-on-error (client fun error)
  (ignore client fun)
  (lime-message "LIME error: " (slime-prin1-to-string error))
  ;(debug)
  )

;;; Client sends the result over a websocket. Handling responses is implemented
;;; by SLIME-NET-FILTER. As we can see, the flow is reversed in our case.
(cl-defun lime-handle-message (client frame)
  (let ((process (lime-client-process client))
        (data (websocket-frame-text frame)))
    (lime-message "LIME-RECV: " data)
    (slime-net-filter process data)))

;;; SLIME calls WEBSOCKET-SEND-STRING against our process -- this function is
;;; left here for testing.
(cl-defun lime-handle-request (process data)
  (lime-message "LIME-SEND: " data)
  (lime-send-text (lime-process-client process) data))

(cl-defun lime-net-listen (host port &rest parameters)
  (when *lime-server*
    (error "LIME server has already started"))
  (setq *lime-server*
        (apply 'websocket-server port
               :host host
               :on-open    (function lime-add-client)
               :on-close   (function lime-del-client)
               :on-error   (function lime-on-error)
               :on-message (function lime-handle-message)
               parameters))
  (unless (memq 'lime-disconnect slime-net-process-close-hooks)
    (push 'lime-disconnect slime-net-process-close-hooks))
  (let ((buf (get-buffer-create "*lime-server*")))
    (set-process-buffer *lime-server* buf)
    (lime-message "Welcome " *lime-server* "!")
    t))

(cl-defun lime-stop ()
  (when *lime-server*
   (websocket-server-close *lime-server*)
   (setq *lime-server* nil)))

;; (lime-net-listen "localhost" 8889)
;; (lime-stop)

(load "/Users/jasonchandler/wecl-build/wecl/Code/slime/slime.el")

(defvar *lime-on* nil)

(defun lime-command ()
  (interactive)
  (if *lime-on*
      (progn
	(setq *lime-on* nil)
	(message (prin1-to-string (lime-stop))))
    (progn
      (setq *lime-on* t)
      (message (prin1-to-string (lime-net-listen "localhost" 8889))))))

*lime-on*

(bind-key* "C-M-l" #'lime-command)
