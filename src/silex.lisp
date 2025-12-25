(in-package "EU.TURTLEWARE.WECL/IMPLEMENTATION")

;;; This file mocks some CLIM interfaces and implements parts of the backend
;;; protocl for HTML/JS/CSS. Later we will swap the first part with real CLIM
;;; and the latter will stay as part of the final implementation

;;; "Borrowed" protocols

(defclass port () ())
(defclass sheet () ())

(defgeneric port (instance))
(defgeneric graft (instance))


;;; 7.2 Sheet Relationship Functions
(defgeneric sheet-parent (sheet))
(defgeneric sheet-children (sheet))
(defgeneric sheet-adopt-child (sheet child))
(defgeneric sheet-disown-child (sheet child &key errorp))

(defgeneric raise-sheet (sheet)
  (:method (sheet)
    (raise-mirror (port sheet) sheet)))

(defgeneric bury-sheet (sheet)
  (:method (sheet)
    (bury-mirror (port sheet) sheet)))

;;; 7.3 Sheet geometry
(defgeneric move-sheet (sheet x y))
(defgeneric resize-sheet (sheet w h))

(defgeneric move-and-resize-sheet (sheet x y w h)
  (:method (sheet x y w h)
    (move-sheet sheet x y)
    (resize-sheet sheet w h)))

;;; 8.1 Input Protocol
(defgeneric distribute-event (port event))

(defgeneric dispatch-event (sheet event)
  (:method (sheet event)
    (handle-event sheet event)))

(defgeneric handle-event (sheet event)
  (:method (sheet event)))

;;; 8.4 Repaint Protocol
(defgeneric repaint-sheet (sheet region)
  (:method (sheet region)
    (handle-repaint sheet region)
    (map nil (lambda (child)
               (repaint-sheet child region))
         (sheet-children sheet))))

(defgeneric dispatch-repaint (sheet region)
  (:method (sheet region)
    (repaint-sheet sheet region)))

(defgeneric handle-repaint (sheet region)
  (:method (sheet region)))

;;; 9.4 Mirrors and Mirrored Sheets

(defgeneric sheet-mirror (sheet))
(defgeneric realize-mirror (port sheet))
(defgeneric destroy-mirror (port sheet))

(defgeneric raise-mirror (port sheet))
(defgeneric bury-mirror (port sheet))

;;; Borrowed "base" classes (clim defines basic-*, but we may decide to do
;;; something custom here).

(defclass base-sheet (sheet)
  ((mirror :initarg :mirror :accessor sheet-mirror)
   (parent :initform nil :accessor sheet-parent)
   (children :initform nil :accessor sheet-children)))

;;; Borrowed event classes
(defclass event () ())

;;; FIXME modifier-state
(defclass device-event (event)
  ((sheet :initarg :sheet :accessor event-sheet)
   (modifier-state :initarg :modifier-state :reader event-modifier-state)
   (mirror :initarg :mirror :accessor event-mirror)
   (source :initarg :source :accessor event-source))
  (:default-initargs :sheet nil :modifier-state 0 :mirror nil :source nil))

(defclass pointer-event (device-event)
  ((x :initarg :x :reader pointer-event-native-x)
   (y :initarg :y :reader pointer-event-native-y)
   (button :initarg :button :reader pointer-event-button)))

(defmethod print-object ((self pointer-event) stream)
  (print-unreadable-object (self stream :type nil :identity nil)
    (format stream "~a ~a ~a"
            (class-name (class-of self))
            (pointer-event-native-x self)
            (pointer-event-native-y self))))

(defclass pointer-motion-event (pointer-event) ())
(defclass pointer-button-event (pointer-event) ())
(defclass pointer-button-press-event (pointer-button-event) ())
(defclass pointer-button-release-event (pointer-button-event) ())

(defclass keyboard-event (device-event)
  ((key-name :initarg :key-name :reader keyboard-event-key-name)
   (key-char :initarg :key-char :reader keyboard-event-character)))

(defmethod print-object ((self keyboard-event) stream)
  (print-unreadable-object (self stream :type nil :identity nil)
    (format stream "~a ~s ~s"
            (class-name (class-of self))
            (keyboard-event-key-name self)
            (keyboard-event-character self))))

(defclass key-press-event (keyboard-event) ())
(defclass key-release-event (keyboard-event) ())

;;; Borrowed constants (mocks)
(defvar +everywhere+ t)
(defvar +nowhere+ nil)


;;; Everything below this point is wecl-specific.

(define-js-accessor (mirror-sheet :js-expr "sheet" :type :object)
    ((obj :js-ref)
     (val :object)))

(defclass wecl-port (port)
  ((connection :initform [document] :accessor connection)
   (focused-sheet :initform nil :accessor focused-sheet)
   (pressed-sheet :initform nil :accessor pressed-sheet)
   (initialized-p :initform nil :accessor initialized-p)))

(defclass wecl-graft (wecl-sheet)
  ((port :initform +wecl-port+)
   (mirror :initform [body])))

(defclass wecl-sheet (base-sheet)
  ((class :initarg :class) #|kludge|#))

(defvar +wecl-port+ (make-instance 'wecl-port))
(defvar +wecl-graft+ (make-instance 'wecl-graft))

(defun find-port (&rest args) +wecl-port+)
(defun find-graft (&rest args) +wecl-graft+)

(defmethod port (client) +wecl-port+)
(defmethod graft (client) +wecl-graft+)

;;; Port
(defun register-js-event-callback (port sheet event-type)
  (let ((callback
          (lambda-js-callback :null ((event :js-ref))
            (|stopPropagation| event)
            (let ((mirror (|target| event)))
              (when (mirror-sheet mirror)
                (|preventDefault| event)))
            (distribute-event port (appropriate-js-event event-type event)))))
    (add-event-listener [document] event-type callback)))

(defun sensitize-port (port sheet)
  (unless (initialized-p port)
    (register-js-event-callback port sheet [mousemove])
    (register-js-event-callback port sheet [mousedown])
    (register-js-event-callback port sheet [mouseup])
    (register-js-event-callback port sheet [keyup])
    (register-js-event-callback port sheet [keydown])
    (setf (initialized-p port) t)))

;;; Sheet
(defmethod initialize-instance :after ((sheet wecl-sheet) &key mirror)
  (setf (mirror-sheet mirror) sheet)
  (repaint-sheet sheet +everywhere+))

(defmethod sheet-adopt-child ((sheet wecl-sheet) child)
  (setf (sheet-parent child) sheet)
  (push child (sheet-children sheet))
  (append-child (sheet-mirror sheet) (sheet-mirror child)))

(defmethod sheet-disown-child ((sheet wecl-sheet) child &key (errorp t))
  (unless (eql (sheet-parent child) sheet)
    (error "~s is not a child of ~s." child sheet))
  (setf (sheet-parent child) nil)
  (deletef (sheet-children sheet) child)
  (remove-child (sheet-mirror sheet) (sheet-mirror child)))

(defun find-sheetened-mirror (mirror)
  (let ((sheet (mirror-sheet mirror)))
    (if (typep sheet 'sheet)
        (values sheet mirror)
        (ext:if-let ((parent (parent-node mirror)))
          (find-sheetened-mirror parent)
          (format *js-console* "giving up at ~a~%" (js-load-str mirror))))))

;;; Events
(defun make-pointer-event (class event)
  (let ((mirror (|target| event)))
    (make-instance class :x (|pageX| event)
                         :y (|pageY| event)
                         :button (|button| event)
                         :sheet (mirror-sheet mirror)
                         :mirror mirror
                         :source event)))

(defun make-keyboard-event (class event)
  (let ((mirror (|target| event)))
    (multiple-value-bind (name char) (normalize-keyboard-event event)
      (make-instance class :key-name name
                           :key-char char
                           :sheet (mirror-sheet mirror)
                           :mirror mirror
                           :source event))))



(defmethod distribute-event ((port wecl-port) event)
  (format *js-console* "~a: ~a???~%" port event))

(defmethod distribute-event ((port wecl-port) (event pointer-event))
  (ext:when-let ((pressed-sheet (pressed-sheet port)))
    (setf (event-sheet event) pressed-sheet))
  (dispatch-event (event-sheet event) event))

(defmethod distribute-event :around ((port wecl-port) (event pointer-event))
  (if (or (pressed-sheet port)
          (typep (event-sheet event) 'sheet))
      (call-next-method)
      (multiple-value-bind (sheet mirror) (find-sheetened-mirror (event-mirror event))
        (if sheet
            (setf (event-sheet event) sheet
                  (event-mirror event) mirror)
            (setf (event-sheet event) +wecl-graft+
                  (event-mirror event) (sheet-mirror +wecl-graft+)))
        (call-next-method))))

(defmethod distribute-event :before ((port wecl-port) (event pointer-button-press-event))
  (unless (pressed-sheet port)
    (setf (pressed-sheet port) (event-sheet event)))
  (setf (focused-sheet port) (event-sheet event)))

(defmethod distribute-event :after ((port wecl-port) (event pointer-button-release-event))
  (setf (pressed-sheet port) nil))

(defmethod distribute-event ((port wecl-port) (event keyboard-event))
  (ext:when-let ((focused-sheet (focused-sheet port)))
    (setf (event-sheet event) focused-sheet))
  (dispatch-event (event-sheet event) event))

;;; Bubble up
(defmethod handle-event ((sheet wecl-sheet) event)
  (ext:when-let ((parent (and sheet (sheet-parent sheet))))
    (handle-event parent event)))


;;; Everything below this point is for the demo.

;;; Helper macro for concatenating CSS attributs.
(defmacro set-css-style ((element) &body body)
  `(macrolet ((attr (test fmt &rest args)
                `(if ,test (format nil ,fmt ,@args) "")))
     (set-attribute ,element "style" (concatenate 'string ,@body))))

;;; Curbed from https://fonts.google.com/icons

(defparameter *add-icon* "
<svg height='1.5em' viewBox='0 -960 960 960' fill='#222222' style='display:block'>
  <path d='M440-440H200v-80h240v-240h80v240h240v80H520v240h-80v-240Z'/>
</svg>")

(defparameter *close-icon* "
<svg height='1.5em' viewBox='0 -960 960 960' fill='#222222' style='display:block'>
  <path d='m256-200-56-56 224-224-224-224 56-56 224 224 224-224 56 56-224 224 224 224-56 56-224-224-224 224Z'/>
</svg>")

(defparameter *resize-border-icon* "
<svg height='1.5em' viewBox='0 -960 960 960' fill='#222222' style='display:block'>
  <path d='M280-120v-80h80v80h-80Zm160 0v-80h80v80h-80Zm160 0v-80h80v80h-80Zm160 0v-80h80v80h-80Zm0-160v-80h80v80h-80Zm0-160v-80h80v80h-80Zm0-160v-80h80v80h-80ZM120-120v-720h720v80H200v640h-80Z'/>
</svg>")

(defparameter *fullscreen-icon* "
<svg height='1.5em' viewBox='0 -960 960 960' fill='#222222' style='display:block'>
  <path d='M120-120v-200h80v120h120v80H120Zm520 0v-80h120v-120h80v200H640ZM120-640v-200h200v80H200v120h-80Zm640 0v-120H640v-80h200v200h-80Z'/>
</svg>")

(defparameter *fullscreen-exit-icon* "
<svg height='1.5em' viewBox='0 -960 960 960' fill='#222222' style='display:block'>
  <path d='M240-120v-120H120v-80h200v200h-80Zm400 0v-200h200v80H720v120h-80ZM120-640v-80h120v-120h80v200H120Zm520 0v-200h80v120h120v80H640Z'/>
</svg>")

(defparameter *maximize-icon* "
<svg height='1.5em' viewBox='0 -960 960 960' fill='#222222' style='display:block'>>
  <path d='M160-760v-80h640v80H160Z'/>
</svg>")

(defparameter *minimize-icon* "
<svg height='1.5em' viewBox='0 -960 960 960' fill='#222222' style='display:block'>
  <path d='M240-120v-80h480v80H240Z'/>
</svg>")


;;; Window utilities
(defclass window-pane (wecl-sheet) ())
(defclass viewport-pane (wecl-sheet) ())

(defun find-window (sheet)
  (etypecase sheet
    ((or wecl-graft null) nil)
    (window-pane sheet)
    (wecl-sheet (find-window (sheet-parent sheet)))))

(defun find-pane-of-type (sheet type)
  (dolist (child (sheet-children sheet))
    (when (typep child type)
      (return-from find-pane-of-type child))))

(defun find-viewport (sheet)
  (find-pane-of-type sheet 'viewport-pane))



;;; todo make it input editing stream
;;; todo stream protocols
;;; todo rescan

;;; The principle behind this "stream" is as follows:
;;;
;;; There is an opened line that is editable. When the user activates the stream
;;; with [return], that line is turned as not editable, we call the activation
;;; callback, and finally we open a new line. That should give us enough room to
;;; implement a simple-minded REPL.
(defclass wecl-stream-pane (viewport-pane)
  ((open-line :initform nil :accessor current-text-line)
   ;; Not really a gadget, but oh well, this makes sense I suppose(!)
   (callback :initarg :stream-callback :reader stream-callback))
  (:default-initargs :stream-callback 'evaluate-string))

(defun evaluate-string (sheet string)
  (flet ((read* (string)
           (handler-case (read-from-string string)
             (error (c)
               (insert-noise-string sheet "READ: ~a~%" c))))
         (eval* (form)
           (handler-case (eval form)
             (error (c)
               (insert-noise-string sheet "EVAL: ~a~%" c))))
         (print* (results)
           (insert-noise-string sheet "~{~a~%~}" results)))
    (print* (multiple-value-list (eval* (read* string))))))

(defun insert-noise-string (sheet string &rest args)
  (let ((line (make-element "span")))
    (setf (inner-text line) (apply #'format nil string args))
    (append-child (sheet-mirror sheet) line)))

;;; Faux output record.
(defun open-text-line (sheet)
  (let ((line (make-element "span")))
    (set-attribute line "contentEditable" "true")
    (set-attribute line "tabindex" "0")
    (append-child (sheet-mirror sheet) line)
    (setf (current-text-line sheet) line)))

(defun close-text-line (sheet)
  (when-let ((line (current-text-line sheet)))
    (setf (current-text-line sheet) nil)
    (set-attribute line "contentEditable" "false")
    (append-child (sheet-mirror sheet) (make-element "br"))
    (inner-text line)))

(defun activate-stream (sheet)
  (when-let ((text (close-text-line sheet)))
    (funcall* (stream-callback sheet) sheet text)))

(defmethod handle-repaint ((sheet wecl-stream-pane) region)
  (unless (current-text-line sheet)
    (insert-noise-string sheet "~a> " (package-name *package*))
    (open-text-line sheet)
    (focus* (current-text-line sheet))))

(defmethod handle-event ((sheet wecl-stream-pane) (event pointer-button-press-event))
  (when-let ((line (current-text-line sheet)))
    (focus* line)))

(defmethod handle-event ((sheet wecl-stream-pane) (event key-press-event))
  (case (keyboard-event-key-name event)
    (:newline
     (|preventDefault| (event-source event))
     (activate-stream sheet)
     (dispatch-repaint sheet +everywhere+))))


(defclass wecl-gadget (wecl-sheet)
  ((armed :initform nil)))

(defclass push-button-pane (wecl-gadget)
  ((label :initform nil)
   (icon :initarg :icon :initform nil)
   (callback :initarg :activate-callback :accessor gadget-activate-callback))
  (:default-initargs :activate-callback nil))

(defmethod handle-repaint ((sheet push-button-pane) region)
  (let ((mirror (sheet-mirror sheet)))
    (setf (inner-html mirror) (slot-value sheet 'icon))))

(defmethod handle-event ((sheet push-button-pane) (event pointer-button-press-event))
  (with-slots (armed) sheet
    (setf armed t)))

(defmethod handle-event ((sheet push-button-pane) (event pointer-button-release-event))
  (with-slots (armed callback) sheet
    (when armed
      (setf armed nil)
      (funcall* callback sheet))))


(defclass drag-item-gadget (wecl-gadget)
  ((drag-x0 :initform 0)
   (drag-y0 :initform 0)
   (drag-callback :initarg :drag-callback :accessor item-drag-callback)))

(defmethod handle-event ((sheet drag-item-gadget) (event pointer-button-press-event))
  (with-slots (armed drag-x0 drag-y0) sheet
    (setf armed t)
    (setf drag-x0 (pointer-event-native-x event))
    (setf drag-y0 (pointer-event-native-y event))))

(defmethod handle-event ((sheet drag-item-gadget) (event pointer-button-release-event))
  (with-slots (armed) sheet
    (setf armed nil)))

(defmethod handle-event ((sheet drag-item-gadget) (event pointer-motion-event))
  (with-slots (armed drag-x0 drag-y0 drag-callback) sheet
    (when armed
      (let ((x1 (pointer-event-native-x event))
            (y1 (pointer-event-native-y event)))
        (funcall* drag-callback sheet drag-x0 drag-y0 x1 y1)))))


(defclass title-bar-gadget (drag-item-gadget)
  ((title :initarg :label)
   (window-dx :initform 0)
   (window-dy :initform 0))
  (:default-initargs :drag-callback 'move-window-callback))

(defmethod handle-repaint ((sheet title-bar-gadget) region)
  (declare (ignore region))
  (let ((mirror (sheet-mirror sheet)))
    (setf (inner-text mirror) (slot-value sheet 'title))))

(defmethod handle-event :after ((sheet title-bar-gadget) (event pointer-button-press-event))
  (with-slots (armed drag-x0 drag-y0 window-dx window-dy) sheet
    (when armed
     (ext:when-let ((window (find-window sheet)))
       (multiple-value-bind (window-x window-y) (bounding-rectangle-position window)
         (setf window-dx (- drag-x0 window-x))
         (setf window-dy (- drag-y0 window-y)))))))

(defun move-window-callback (sheet x0 y0 x1 y1)
  (declare (ignore x0 y0))
  (with-slots (window-dx window-dy) sheet
    (ext:when-let ((window (find-window sheet)))
      (let ((mirror (sheet-mirror window)))
        (setf (element.x1 mirror) (format nil "~Apx" (- x1 window-dx)))
        (setf (element.y1 mirror) (format nil "~Apx" (- y1 window-dy)))))))


(defclass resize-window-gadget (drag-item-gadget push-button-pane)
  ((window-ws :initform 0)
   (window-hs :initform 0))
  (:default-initargs :drag-callback 'resize-window-callback :icon *resize-border-icon*))

(defmethod handle-event :after ((sheet resize-window-gadget) (event pointer-button-press-event))
  (with-slots (armed window-ws window-hs) sheet
    (when armed
      (ext:when-let ((window (find-window sheet)))
        (multiple-value-bind (window-w window-h) (bounding-rectangle-size window)
          (setf window-ws window-w)
          (setf window-hs window-h))))))

(defun resize-window-callback (sheet x0 y0 x1 y1)
  (with-slots (window-ws window-hs) sheet
    (ext:when-let ((window (find-window sheet)))
      (let ((mirror (sheet-mirror window))
            (dx (- x1 x0))
            (dy (- y1 y0)))
        (setf (element.ws mirror) (format nil "~Apx" (max (+ window-ws dx) 100)))
        (setf (element.hs mirror) (format nil "~Apx" (max (+ window-hs dy) 100)))))))


;;; Sheet constructors

(defun make-window (class &rest args
                    &key (left     0 pos-x1-p)
                         (top      0 pos-y1-p)
                         (right    0 pos-x2-p)
                         (bottom   0 pos-y2-p)
                         (width  400 size-w-p)
                         (height 400 size-h-p)
                         (borders t)
                    &allow-other-keys)
  (let ((new (make-element "div")))
    (set-css-style (new)
      (attr t "position:fixed;")
      (attr pos-x1-p "left:~Apx;" left)
      (attr pos-y1-p "top:~Apx;" top)
      (attr pos-x2-p "right:~Apx;" right)
      (attr pos-y2-p "bottom:~Apx;" bottom)
      (attr (or size-w-p (null pos-x2-p)) "width:~Apx;" width)
      (attr (or size-h-p (null pos-y2-p)) "height:~Apx;" height)
      (attr borders "border:solid;"))
    (make-instance class :mirror new)))

(defun make-push-button (class &rest args)
  (let ((new (make-element "div")))
    (set-css-style (new)
      (attr t "position:static;")
      (attr t "float:right;"))
    (apply #'make-instance class :mirror new args)))

(defun make-side-bar (class side size &rest args)
  (let ((new (make-element "div")))
    (set-css-style (new)
      (attr t "position:absolute;")
      (attr t "background:#ddddff;")
      (attr t "padding-left: 5px;")
      (attr t "user-select: none;")
      (attr t "overflow: clip;")
      (attr t "text-wrap: nowrap;")
      (attr (not (eql side :top))    "bottom:0;")
      (attr (not (eql side :left))   "right:0;")
      (attr (not (eql side :right))  "left:0;")
      (attr (not (eql side :bottom)) "top:0;")
      (attr (member side '(:top :bottom)) "height:~A;" size)
      (attr (member side '(:left :right)) "width:~A;" size)
      (attr (eql side :left)   "border-right: solid;")
      (attr (eql side :top)    "border-bottom: solid;")
      (attr (eql side :right)  "border-left: solid;")
      (attr (eql side :bottom) "border-top: solid;"))
    (apply #'make-instance class :mirror new args)))

(defun make-viewport (class &rest args)
  (let ((new (make-element "div")))
    (set-css-style (new)
      (attr t "position:absolute;")
      (attr t "background:#ddffff;")
      (attr t "overflow: scroll;")
      ;; (attr t "user-select: none;")
      ;; (attr t "opacity: 0.8;")
      (attr t "left:0;")
      (attr t "top:1.5em;")
      (attr t "right:0;")
      (attr t "bottom:1.5em;"))
    (apply #'make-instance class :mirror new args)))

(defun make-pane (type &rest args &key class &allow-other-keys)
  (ecase type
    (:window
     (apply #'make-window (or class 'window-pane) args))
    (:push-button
     (apply #'make-push-button (or class 'push-button-pane) args))
    (:title-bar
     (apply #'make-side-bar (or class 'title-bar-gadget) args))
    (:status-bar
     (apply #'make-side-bar (or class 'wecl-sheet) args))
    (:resize-gadget
     (apply #'make-push-button (or class 'resize-window-gadget) args))
    (:viewport
     (apply #'make-viewport (or class 'viewport-pane) args))))

(defun open-window-stream (&rest args)
  (let* ((port (find-port))
         (graft (find-graft))
         (window (apply #'make-pane :window args))
         (title-bar  (make-pane :title-bar :top "1.5em" :label "Window Title"))
         (status-bar (make-pane :status-bar :bottom "1.5em"))
         (viewport  (make-pane :viewport :class 'wecl-stream-pane))
         (button-1  (make-pane :push-button :icon *close-icon*
                                            :activate-callback (lambda (gadget)
                                                                 (sheet-disown-child graft window))))
         (button-2  (make-pane :push-button :icon *add-icon*
                                            :activate-callback
                                            (lambda (gadget)
                                              (apply #'open-window-stream
                                                     :left (random 300)
                                                     :top (random 300)
                                                     :width 640 :height 480 args))))
         (button-3  (make-pane :resize-gadget)))
    (sensitize-port port graft)
    (sheet-adopt-child window viewport)
    (sheet-adopt-child window title-bar)
    (sheet-adopt-child window status-bar)
    (sheet-adopt-child title-bar button-1)
    (sheet-adopt-child title-bar button-2)
    (sheet-adopt-child status-bar button-3)
    (sheet-adopt-child graft window)
    window))



(progn
  (clear-children [body])
  (open-window-stream :class 'window-pane :left 125 :top 125 :height 600 :width 800))
