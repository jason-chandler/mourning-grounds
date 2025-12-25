(in-package "EU.TURTLEWARE.WECL/IMPLEMENTATION")

;;; Utilities.
(defun curry (fun &rest args)
  (lambda (&rest other-args)
    (apply fun (append args other-args))))

(defun recur (n)
  (when (>= n 0)
    (format *error-output* "Recur: ~d~%" n)
    (recur (1- n))))

(defvar *id* 0)
(defun unique-id (name)
  (format nil "wecl-~a-~d" name (incf *id*)))

;;; Converts a character to a "safe" string for javascript.
;;; TODO add the same thing for strings. Ensure correcness.
(defun sanitize (ch)
  (case ch
    (#\newline "\\n")
    (#\'       "\\'")
    (#\`       "\\`")
    (#\\       "\\\\")
    (#\#       "\\#")
    (otherwise (string ch))))

(defun print-file (file)
  (with-open-file (input file)
    (format t "-- ~a~%" file)
    (loop for idx from 0
          for line = (read-line input nil)
          while line do
            (format t "~4,'0d: ~a~%" idx line)
            (js-yield*))
    (format t "++ ~a~%" file)))

(defun read-file-to-string (file)
  (with-output-to-string (output)
    (with-open-file (input file)
      (loop for line = (read-line input nil)
            while line do
              (write-string line output)))))

(defun read-file-to-js-string (file)
  (with-output-to-string (output)
    (with-open-file (input file)
      (loop for line = (read-line input nil)
            while line do
              (loop for ch across line do
                (write-string (sanitize ch) output))
              (terpri output)))))

(defun read-delimited-string (terminating-char stream)
  (with-output-to-string (str)
    (loop for ch = (read-char stream)
          until (char= ch terminating-char)
          do (princ ch str))))

;;; FIXME don't use JS-EVAL*, because syntax error could crash our context.
;;; Create a separate context for evaluating js expressions.
(set-dispatch-macro-character
 #\# #\j
 (lambda (stream subchar argument)
   (declare (ignore subchar argument))
   `(js-eval* ,(read-delimited-string #\# stream))))


;;; TODO we should pull charming to operate escape codes. For now I'm only
;;; copying relevant parts.

(defun write1 (arg)
  (princ arg))

(defun put (&rest args)
  "Put raw string on a terminal"
  (map nil #'write1 args))

(defun esc (&rest args)
  "Escape sequence"
  (apply #'put (code-char #x1b) args))

(defun csi (&rest args)
  "Control sequence introducer"
  (apply #'esc #\[ args))

(defun reset-terminal ()
  "Clears the screen, attributes, cursor position etc."
  ;; (setf *terminal-foreground* #xffbbbbbb)
  ;; (setf *terminal-background* #xff111111)
  ;; (setf *terminal-text-attributes* nil)
  ;; (setf *terminal-mouse-tracked-p* nil)
  ;; (setf *terminal-cursor-visible-p* nil)
  (esc "c"))

(defun save-cursor-position ()
  (csi "s"))

(defun restore-cursor-position ()
  (csi "u"))

(defun clear-terminal (&optional (mode 2))
  "Erase in display"
  ;; Defined modes:
  ;; 0 - clear from cursor to the end of the display
  ;; 1 - clear from cursor to the start of the display
  ;; 2 - clear entire display
  (csi mode "J"))

(defun clear-line (&optional (mode 2))
  "Erase in line."
  ;; Defined modes:
  ;; 0 - clear from cursor to the end of the line
  ;; 1 - clear from cursor to the start of the line
  ;; 2 - clear entire line
  (csi mode "K"))


;;; Simple-minded REPL

#+ (or)
(progn
  (defun rep ()
    (format *query-io* "~&~a> " (package-name *package*))
    (shiftf +++ ++ + - (read *query-io* nil '%quit))
    (when (eq - '%quit)
      (throw :exit "bye!"))
    (shiftf /// // / (multiple-value-list (eval -)))
    (shiftf *** ** * (first /))
    (format *query-io* "~&~{ ~s~^~%~}~%" /))

  (defun repl ()
    (catch :exit
      (loop (handler-case (rep)
              (condition (c)
                (format *query-io* "~&~a~%~a~%" (class-name (class-of c)) c)))))))

(defun repl ()
  (si::tpl :quiet t))


;;; TODO we should pull here INPUT-EDITING-STREAM from CLIM and celebrate the
;;; fact that we already have a protocol for this use case. In a meantime...
(defclass edited-string ()
  ((state :initform nil)
   (data-string :initform (make-array 64 :fill-pointer 0 :adjustable t))
   (scan-ptr :initform 0)
   (edit-ptr :initform 0)
   (fill-ptr :initform 0)))

(defmethod print-object ((self edited-string) stream)
  (with-slots (data-string fill-ptr scan-ptr edit-ptr) self
    (format stream "#\"~a>~a|~a<~a\""
            (coerce (subseq data-string 0 scan-ptr) 'string)
            (coerce (subseq data-string scan-ptr edit-ptr) 'string)
            (coerce (subseq data-string edit-ptr fill-ptr) 'string)
            (coerce (subseq data-string fill-ptr) 'string))))

(defun beep ()
  (format *error-output* "~&(audible beep)~%"))

(defun clear-buffer (self)
  (with-slots (state data-string fill-ptr scan-ptr edit-ptr) self
    (setf state nil
          scan-ptr 0
          edit-ptr 0
          fill-ptr 0
          (fill-pointer data-string) 0)))

(defun activate (self)
  (declare (ignore self))
  ;; (format *error-output* "~&(activation)~%")
  (throw 'activate nil))

(defun incf-cursor (estr)
  (with-slots (data-string fill-ptr scan-ptr edit-ptr) estr
    (if (< edit-ptr fill-ptr)
        (progn
          (incf edit-ptr)
          (format t "~a[C" #\escape))
        (beep))))

(defun decf-cursor (estr)
  (with-slots (data-string fill-ptr scan-ptr edit-ptr) estr
    (if (> edit-ptr scan-ptr)
        (progn
          (decf edit-ptr)
          (format t "~a[D" #\escape))
        (beep))))

(defun process-advance (estr)
  (with-slots (data-string fill-ptr scan-ptr edit-ptr) estr
    (if (< scan-ptr fill-ptr)
        (prog1 (aref data-string scan-ptr)
          (incf scan-ptr)
          (cond
            ((= scan-ptr fill-ptr)
             (setf scan-ptr 0)
             (setf edit-ptr 0)
             (setf fill-ptr 0)
             (setf (fill-pointer data-string) 0))
            ((> scan-ptr edit-ptr)
             (setf edit-ptr scan-ptr))))
        (beep))))

(defun process-insert (estr ch)
  (with-slots (data-string fill-ptr scan-ptr edit-ptr) estr
    (if (= edit-ptr fill-ptr)
        (progn
          (vector-push-extend ch data-string)
          (incf fill-ptr)
          (incf edit-ptr)
          (princ ch))
        (let ((rest-str (coerce (subseq data-string edit-ptr) 'string)))
          (vector-push-extend #\nul data-string)
          (setf (aref data-string edit-ptr) ch)
          (incf fill-ptr)
          (incf edit-ptr)
          (replace data-string rest-str :start1 edit-ptr)
          (clear-line 0)
          (princ ch)
          (save-cursor-position)
          (princ rest-str)
          (restore-cursor-position)))))

;;; TODO support editing of multiple lines.
(defun process-newline (estr)
  (with-slots (edit-ptr fill-ptr) estr
    (setf edit-ptr fill-ptr)
    (process-insert estr #\newline)
    (activate estr)))

(defun process-backspace (estr)
  (with-slots (data-string fill-ptr scan-ptr edit-ptr) estr
    (cond
      ((= edit-ptr scan-ptr)
       nil)
      ((= edit-ptr fill-ptr)
       (vector-pop data-string)
       (decf fill-ptr)
       (decf edit-ptr)
       (format t "~a ~a" #\backspace #\backspace))
      ((let ((rest-str (coerce (subseq data-string edit-ptr) 'string)))
         (vector-pop data-string)
         (decf fill-ptr)
         (decf edit-ptr)
         (replace data-string rest-str :start1 edit-ptr)
         (princ #\backspace)
         (clear-line 0)
         (save-cursor-position)
         (princ rest-str)
         (restore-cursor-position))))))

(defun process-escape (self ch)
  (with-slots (state) self
    (flet ((reject-state ()
             (prog1 t
               (format *error-output* "Unknown ~s -- ~s~%" state ch)
               (setf state nil)))
           (update-state (new-state)
             (prog1 t
               (setf state new-state)))
           (normal-state ()
             nil))
      (ecase state
        ((nil)
         (case ch
           (#\escape
            (update-state :esc))
           (otherwise
            (normal-state))))
        (:esc
         (case ch
           (#\[
            (update-state :csi))
           (otherwise
            (reject-state))))
        (:csi
         (case ch
           (#\D
            (decf-cursor self)
            (update-state nil))
           (#\C
            (incf-cursor self)
            (update-state nil))
           ;; (#\A (history-up self))
           ;; (#\B (history-dn self))
           (otherwise
            (reject-state))))))))

(defun process-gesture (self ch)
  (cond
    ((process-escape self ch))
    ((graphic-char-p ch)
     (process-insert self ch))
    ((or (char= ch #\backspace)
         (char= ch #\rubout))
     (process-backspace self))
    ((char= ch #\return)
     (process-newline self))
    (t
     (format *error-output* "~&Unknown character ~s~%" ch)))
  #+ (or)
  (format *error-output* "~a~%" self))

(defun read-next-gesture (self)
  (let ((buffer (slot-value self 'buffer))
        (js-var (slot-value self 'js-var))
        (*standard-output* self))
    (with-slots (data-string fill-ptr scan-ptr edit-ptr) buffer
      (tagbody
       :again
         (when (< scan-ptr fill-ptr)
           (return-from read-next-gesture
             (process-advance buffer)))
         (catch 'activate
           (loop
            (loop for chr across (js-read* js-var)
                  for obj = (process-gesture buffer chr))))
         (go :again)))))



(defclass js-xterm-stream (gray:fundamental-character-input-stream
                           gray:fundamental-character-output-stream)
  ((buffer :initform (make-instance 'edited-string))
   (unread :initform '())
   (column :initform 0)
   (js-var :initarg :js-var)))

(defmethod print-object ((self js-xterm-stream) stream)
  (print-unreadable-object (self stream :type nil :identity nil)
    (format stream "js:xterm ~a" (slot-value self 'js-var))))

(defun buffer-read-char (self)
  (let ((buffer (slot-value self 'buffer)))
    (with-slots (data-string fill-ptr scan-ptr edit-ptr) buffer
      (if (< scan-ptr fill-ptr)
          (process-advance buffer)
          (read-next-gesture self)))))

(defmethod gray:stream-listen ((self js-xterm-stream))
  (not (null (slot-value self 'unread))))


(defmethod gray:stream-read-char ((self js-xterm-stream))
  (or (pop (slot-value self 'unread))
      (buffer-read-char self)))

(defmethod gray:stream-read-char-no-hang ((self js-xterm-stream))
  (pop (slot-value self 'unread)))

(defmethod gray:stream-unread-char ((self js-xterm-stream) ch)
  (push ch (slot-value self 'unread)))

(defmethod gray:stream-write-char ((self js-xterm-stream) ch)
  (with-slots (js-var column) self
    (case ch
      (#\newline
       (js-eval* "~a.write(ostr_nl);" js-var)
       (setf column 0))
      ((#\backspace #\rubout)
       (js-eval* "~a.write('\\b');" js-var)
       (decf column))
      (otherwise
       (js-eval* "~a.write(`~a`)" js-var (sanitize ch))
       (incf column)))))

(defmethod gray:stream-clear-output ((self js-xterm-stream))
  (let ((*standard-output* self))
    (reset-terminal)
    (setf (slot-value self 'column) 0)))

(defmethod gray:stream-clear-input ((self js-xterm-stream))
  (with-slots (buffer unread) self
    (clear-buffer buffer)
    (setf unread nil)))

(defmethod gray:stream-line-column ((self js-xterm-stream))
  (slot-value self 'column))

(defmethod gray:stream-finish-output ((self js-xterm-stream))
  (js-yield*))

(defmethod gray:stream-force-output ((self js-xterm-stream))
  (js-yield*))


(defvar *ostr-stream* (make-instance 'js-xterm-stream :js-var "ostr_xterm"))
(defvar *repl-stream* (make-instance 'js-xterm-stream :js-var "repl_xterm"))

;;; Code buffer

;;; This one is a banger; we use ed function to populate file onto the text
;;; editor buffer:

(defun update-code-buffer* (string)
  (let ((node (get-element-by-id [document] "code")))
    (setf (text-content node) string)))

(defun update-code-buffer (source)
  (let ((node (get-element-by-id [document] "code"))
        (string (if (probe-file source)
                    (read-file-to-js-string source)
                    (js-wait* "fetch('~a')" source))))
    ;; We need to sanitize text because it may contain # -- this character is
    ;; interpreted even when we wrap the string in ``.
    ;;
    ;; FIXME we should pass here a js-ref instead of marshalling a string.
    (setf (text-content node) (with-output-to-string (str)
                                (loop for c across string do
                                  (princ (sanitize c) str)))))
  t)

(defun invoke-code-buffer ()
  (with-input-from-string (stream (inner-text [code-buffer]))
    (compile-file stream :output-file "code-buffer.fasc" :load t)))

#+(or) ;; not yet (static build? ship fasl? c.f dlopen)
(progn
  (require "ASDF")
  (print (asdf:asdf-version)))



;;; Canvas
(defun open-canvas (&optional (name (unique-id "draw")))
  (let ((pane (open-tile :html-type "canvas" :css-class "tile"
                         :x-node-id name)))
    (js-eval* "
     { canvas = shadow_dom.get('~a');
       canvas.width = 640;
       canvas.height = 400; }"
              (x-node-id pane))
    pane))


;;; 2d context
(defun draw-rectangle/canvas (x1 y1 x2 y2)
  (let ((node (find-sheet "draw")))
    (js-eval* "block:
     { canvas = shadow_dom.get('~a');
       ctx = canvas.getContext('2d');
       if (!ctx) { alert(\"Sucks to be you. 2d context failure.\"); break block; }
       ctx.fillStyle = 'green';
       ctx.fillRect(~a, ~a, ~a, ~a); }"
              (x-node-id node)
              x1 y1 x2 y2)))


(defparameter *gl-initialized-p* nil)

(defun gl-init ()
  (unless *gl-initialized-p*
    (let ((node (find-sheet "draw")))
      (js-eval* "block:
       { canvas = shadow_dom.get('~a');
         gl = canvas.getContext(\"webgl2\");
         if (!gl) { alert(\"Sucks to be you. GL context failure.\"); break block; }}"
                (x-node-id node)))
    (setf *gl-initialized-p* t)
    (gl-clear 0.5 0.5 1.0)))

;;; WebGL context
(defun gl-clear (r g b)
  (js-eval* "block:
   { gl.viewport(0, 0, gl.drawingBufferWidth, gl.drawingBufferHeight);
     gl.clearColor(~f, ~f, ~f, 1.0);
     gl.clear(gl.COLOR_BUFFER_BIT); }"
            r g b))

(defun gl-compile-vshader ()
  (js-eval* "block:
   { vshader = gl.createShader(gl.VERTEX_SHADER);
     gl.shaderSource(vshader, vshader_text);
     gl.compileShader(vshader);
     message = gl.getShaderInfoLog(vshader);
     if (message.length > 0) { throw message; }
     console.log(`vshader OK ` + vshader); }"))

(defun gl-compile-fshader ()
  (js-eval* "block:
   { fshader = gl.createShader(gl.FRAGMENT_SHADER);
     gl.shaderSource(fshader, fshader_text);
     gl.compileShader(fshader);
     message = gl.getShaderInfoLog(fshader);
     if (message.length > 0) { throw message; }
     console.log(`fshader OK ` + fshader); }"))

(defun gl-compile-program ()
  (gl-compile-vshader)
  (gl-compile-fshader)
  (js-eval* "block:
   { program=gl.createProgram();
     gl.attachShader(program, vshader);
     gl.attachShader(program, fshader);
     gl.linkProgram(program);
     message = gl.getShaderInfoLog(fshader);
     if (message.length > 0) { throw message; }
     console.log(`LinkGL OK ` + program); }"))

(defun gl-draw-triangle (x1 y1 x2 y2)
  (js-eval* "block:
   { gl.useProgram(program);

     vertex_arr = new Float32Array([~f, ~f, ~f, ~f, ~f, ~f]);
     vertex_buf = gl.createBuffer();
     gl.bindBuffer(gl.ARRAY_BUFFER, vertex_buf);
     gl.bufferData(gl.ARRAY_BUFFER, vertex_arr, gl.STATIC_DRAW);

     position = gl.getAttribLocation(program, 'position');
     gl.enableVertexAttribArray(position);
     gl.vertexAttribPointer(position, 2, gl.FLOAT, false, 0, 0);
     gl.drawArrays(gl.TRIANGLES, 0, 3); }"
            x1 y1 x1 y2 x2 y1))

(defun gl-demo ()
  (gl-init)
  (gl-compile-program)
  (gl-draw-triangle 0.0 0.0 1.0 1.0)
  (gl-draw-triangle 0.0 0.0 -1.0 -1.0))

(defun gl-demo2 (&optional (sleep-ms 10))
  (gl-init)
  (gl-compile-program)
  ;; this is suboptimal, we should use requestAnimationFrame, see:
  ;; https://developer.mozilla.org/en-US/docs/Web/API/WebGL_API/Basic_2D_animation_example
  (loop for i from -1.0 upto 3.0 by 0.01 do
        (gl-draw-triangle -1 -1 i i)
        (js-sleep sleep-ms)))


;;; Code buffer
(defun open-buffer (&optional (name (unique-id "code")))
  (let ((sheet (find-sheet name)))
    (when sheet
      (return-from open-buffer sheet)))
  (let ((sheet (create-sheet "code" "tile" name)))
    (realize-mirror sheet)
    (js-eval* "
     { graft = shadow_dom.get('graft');
       elder = document.createElement('pre');
       scion = shadow_dom.get('~a');
       scion.contentEditable = true;
       scion.spellcheck = false;
       elder.appendChild(scion); 
       graft.appendChild(elder); }"
              name)
    sheet))


#+ (or)
(defun create-world ()
  (let ((graft (import-sheet "body" "body" "graft"))
        (stdo (import-sheet "div" "" "ostr"))
        (repl (import-sheet "div" "" "repl"))
        (code (open-buffer "code"))
        (draw (open-canvas "draw")))))

(defun wecl-init ()
  (setf *standard-output* *ostr-stream*
        *standard-input*  *ostr-stream*
        *error-output*    *ostr-stream*
        *query-io*        *repl-stream*
        *debug-io*        *repl-stream*
        *terminal-io*     *repl-stream*
        *package* (find-package "EU.TURTLEWARE.WECL/IMPLEMENTATION"))
  (pushnew #'update-code-buffer ext:*ed-functions*)
  (clear-output *repl-stream*)
  #+ (or) (create-world)
  ;; go!
  (repl))

(wecl-init)
