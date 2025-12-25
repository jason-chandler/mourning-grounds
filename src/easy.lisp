(in-package "EU.TURTLEWARE.WECL/IMPLEMENTATION")

(macrolet ((lispify-operator (name)
             `(defalias ,(lispify-name name) ,name))
           (lispify-accessor (name)
             (let ((lisp-name (lispify-name name)))
               `(progn
                  (defalias ,lisp-name ,name)
                  (defalias (setf ,lisp-name) (setf ,name))))))
  (lispify-operator |createElement|)
  (lispify-operator |appendChild|)
  (lispify-operator |removeChild|)
  (lispify-operator |replaceChildren|)
  (lispify-operator |addEventListener|)
  (lispify-operator |elementFromPoint|)
  (lispify-operator |getSelection|)
  (lispify-operator |selectAllChildren|)
  (lispify-operator |collapseToEnd|)
  (lispify-operator |parentNode|)
  (lispify-accessor |innerHTML|)
  (lispify-operator |getElementById|)
  (lispify-operator |querySelectorAll|)
  (lispify-accessor |innerText|)
  (lispify-accessor |textContent|)
  (lispify-operator |setAttribute|)
  (lispify-operator |getAttribute|))

(define-js-variable ([window] :js-expr "window" :type :symbol))
(define-js-object ([root] :js-expr "document" :type :js-ref))
(define-js-object ([body] :js-expr "document.body" :type :js-ref))

(defun clear-children (self)
  (|replaceChildren| self))

(defun focus* (element)
  (let ((selection (get-selection [document])))
    (select-all-children selection element)
    (collapse-to-end selection)))

(define-js-method (focus :js-expr "focus" :type :null)
    ((self :js-ref)))

(define-js-method (click :js-expr "click" :type :null)
    ((self :js-ref)))

;; (define-js-function (js-print :js-expr "console.log" :type :boolean)
;;   ((obj :boolean)))



(defun make-element (type &key class id text)
  (let ((elt (create-element [document] type)))
    (when class (set-attribute elt "class" class))
    (when id (set-attribute elt "id" class))
    (when text (setf (inner-text elt) text))
    elt))

(defun make-paragraph (fmt &rest args)
  (let ((text (apply #'format nil fmt args)))
    (make-element "p" :text text)))

(defun make-listing (name text)
  (let ((div (make-element "div" :class "listing"))
        (par (make-element "p"   :class "listing" :text name))
        (pre (make-element "pre" :class "listing" :text text)))
    (append-child div par)
    (append-child div pre)
    div))

(defun show-script-text (id)
  (let* ((script (get-element-by-id [document] id))
         (src (js-wait* "fetch('~a')" (get-attribute script "src")))
         (head (make-listing "easy.lisp script source" src))
         (body (make-listing "easy.lisp script inline" (inner-text script))))
    (append-child [body] head)
    (append-child [body] body)))

(defun push-counter (start)
  (let* ((txt (format nil "[click me!] Hello World! ~a" start))
         (elt (make-element "p" :text txt))
         (callback (lambda-js-callback :null ((event :js-ref)) ;closure!
                     (incf start)
                     (setf (inner-text elt)
                           (format nil "Hello World! ~a" start)))))
    (add-event-listener elt "click" callback)
    (append-child [body] elt)))

;; 

(defgeneric appropriate-js-event (type event))

(defmacro define-js-event ((name type) (event) &body body)
  `(progn
     (defvar ,name ,type)
     (defmethod appropriate-js-event ((type (eql ,name)) event)
       ,@body)))

;;; keyboard events

;;; https://developer.mozilla.org/en-US/docs/Web/API/Element
(define-js-event ([mousemove] "mousemove") (event)
  (make-pointer-event 'pointer-motion-event event))

(define-js-event ([mousedown] "mousedown") (event)
  (make-pointer-event 'pointer-button-press-event event))

(define-js-event ([mouseup] "mouseup") (event)
  (make-pointer-event 'pointer-button-release-event event))

;;; https://developer.mozilla.org/en-US/docs/Web/API/UI_Events/Keyboard_event_code_values
;;; https://developer.mozilla.org/en-US/docs/Web/API/UI_Events/Keyboard_event_key_values
(defun normalize-keyboard-event (event)
  (let ((code (|code| event))
        (key (|key| event)))
    (if (= 1 (length key))
        (values (intern code "KEYWORD") (char key 0))
        (string-case (key)
          ;; Special
          ("Unidentified" (values :unidentified nil))
          ;; Modifiers
          ("Alt"          (values :alt nil))
          ("AltGraph"     (values :alt-gr nil))
          ("CapsLock"     (values :caps-lock nil))
          ("Control"      (values :control nil))
          ("Fn"           (values :fn nil))
          ("FnLock"       (values :fn-lock nil))
          ("Hyper"        (values :hyper nil))
          ("Meta"         (values :meta nil))
          ("NumLock"      (values :num-lock nil))
          ("ScrollLock"   (values :scroll-lock nil))
          ("Shift"        (values :shift nil))
          ("Super"        (values :super nil))
          ("Symbol"       (values :symbol nil))
          ("SymbolLock"   (values :symbol-lock nil))
          ;; Whitespace
          ("Enter"        (values :newline #\newline))
          ("Tab"          (values :tab #\tab))
          (" "            (values :space #\space))
          ;; Navigation keys
          (("Down" "ArrowDown")   (values :down nil))
          (("Left" "ArrowLeft")   (values :left nil))
          (("Right" "ArrowRight") (values :right nil))
          (("Up" "ArrowUp")       (values :up nil))
          ("End"                  (values :end nil))
          ("Home"                 (values :home nil))
          ("PageDown"             (values :page-down nil))
          ("PageUp"               (values :page-up nil))
          ;; Editing keys (incomplete)
          ("Backspace"       (values :backspace #\backspace))
          ("Clear"           (values :clear nil))
          ("Copy"            (values :copy nil))
          (("Crsel" "CrSel") (values :cr-sel nil))
          ("Cut"             (values :cut nil))
          (("Del" "Delete")  (values :delete #\delete))
          ("EraseEof"        (values :erase-eof nil))
          (("Exsel" "ExSel") (values :ex-sel nil))
          ("Insert"          (values :insert nil))
          ("Paste"           (values :paste nil))
          ("Redo"            (values :redo nil))
          ("Undo"            (values :undo nil))
          ;; Selected exception from other groups
          (("Esc" "Escape")  (values :escape #\escape))
          ("Find"            (values :find nil))
          ("Help"            (values :help nil))
          ;; Many many more keys
          (otherwise      (values :unimplemented nil))))))

(define-js-event ([keydown] "keydown") (event)
  (make-keyboard-event 'key-press-event event))

(define-js-event ([keyup] "keyup") (event)
  (make-keyboard-event 'key-release-event event))



(define-js-getter (element.scroll-x :js-expr "scrollX" :type :fixnum)
    ((self :js-ref)))

(define-js-getter (element.scroll-y :js-expr "scrollY" :type :fixnum)
    ((self :js-ref)))

(define-js-accessor (element.x1 :js-expr "style.left" :type :fixnum)
  ((obj :js-ref)
   (val :string)))

(define-js-accessor (element.y1 :js-expr "style.top" :type :fixnum)
  ((obj :js-ref)
   (val :string)))

(define-js-accessor (element.ws :js-expr "style.width" :type :fixnum)
  ((obj :js-ref)
   (val :string)))

(define-js-accessor (element.hs :js-expr "style.height" :type :fixnum)
  ((obj :js-ref)
   (val :string)))

;;; Bounding rectangle
(define-js-method (element.rect :js-expr "getBoundingClientRect" :type :js-ref)
    ((obj :js-ref)))

(define-js-script (element.rect.x1 :js-expr "~a['left']" :type :fixnum)
    ((obj :js-ref)))
(define-js-script (element.rect.y1 :js-expr "~a['top']" :type :fixnum)
    ((obj :js-ref)))
(define-js-script (element.rect.x2 :js-expr "~a['right']"  :type :fixnum)
    ((obj :js-ref)))
(define-js-script (element.rect.y2 :js-expr "~a['bottom']" :type :fixnum)
    ((obj :js-ref)))

(defun bounding-rectangle* (sheet)
  (let ((rect (element.rect (sheet-mirror sheet)))
        (offx (element.scroll-x [window]))
        (offy (element.scroll-y [window])))
    (values (+ offx (element.rect.x1 rect))
            (+ offy (element.rect.y1 rect))
            (+ offx (element.rect.x2 rect))
            (+ offy (element.rect.y2 rect)))))

(defun bounding-rectangle-position (sheet)
  (let ((rect (element.rect (sheet-mirror sheet)))
        (offx (element.scroll-x [window]))
        (offy (element.scroll-y [window])))
    (values (+ offx (element.rect.x1 rect))
            (+ offy (element.rect.y1 rect)))))

(defun bounding-rectangle-size (sheet)
  (let ((rect (element.rect (sheet-mirror sheet))))
    (values (- (element.rect.x2 rect) (element.rect.x1 rect)) 
            (- (element.rect.y2 rect) (element.rect.y1 rect)) )))
