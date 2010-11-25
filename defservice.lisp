(defpackage :defservice
  (:use :cl :url-encode)
  (:export #:make-start-context #:defservice #:defcontext #:allow-empty-resource-name
           #:dispatch-request #:go-on
           #:url-encode #:url-decode
           #:dispatch-failed #:dispatch-failed-code #:dispatch-failed-message #:dispatch-failed-headers))

(in-package :defservice)

(define-condition dispatch-failed (error)
  ((code :initarg :code :reader dispatch-failed-code)
   (message :initarg :message :reader dispatch-failed-message)
   (headers :initarg :headers :initform () :reader dispatch-failed-headers))
  (:report (lambda (c s) (format s "~a: ~a" (dispatch-failed-code c) (dispatch-failed-message c)))))


(defun not-found (control &rest args)
  (error 'dispatch-failed :code 404 :message (apply #'format nil control args)))
(defun method-not-allowed ()
  (error 'dispatch-failed :code 405 :message "Method not allowed."))
(defun moved-permanently (location)
  (error 'dispatch-failed :code 301 :message (format nil "Canonical URL: ~a" location)
         :headers `(("location" . ,location))))

(defun ensure-list (x) (if (listp x) x (list x)))

(defun method-pos (method)
  (case method (:get 0) (:head 0) (:post 1) (:put 2) (:delete 3)))

(defstruct handler args func nowrap)
(defstruct uri (methods (vector nil nil nil nil)) continue)
(defstruct named-resource wrap uri allow-empty)
(defstruct split paths)
(defstruct tail uri)
(defstruct path name uri wrap)

(defmethod print-object ((x uri) stream)
  (print-unreadable-object (x stream :type t :identity t)))

(defvar *contexts* (make-hash-table))

(defun make-uri-lambda (path args body)
  (let ((n-arg (gensym)))
    `(lambda (,n-arg ,@args)
       (declare (ignorable ,n-arg))
       (let ,(loop :with off := 0 :for elt :in (reverse path)
                   :when (symbolp elt)
                   :collect `(,elt (nth ,off ,n-arg)) :and :do (incf off))
         ,@body))))

(defun make-start-context (name)
  (unless (gethash name *contexts*)
    (setf (gethash name *contexts*) (make-uri))))

(defun get-context (name)
  (or (gethash name *contexts*)
      (error "No context '~a' defined." name)))

(defmacro defservice (methods contexts path args &body body)
  (multiple-value-bind (arg-names arg-specs)
      (loop :for (name . spec) :in args
            :collect (if (consp name) (car name) name) :into names
            :collect (cons (if (consp name) (second name) (string-downcase (symbol-name name)))
                           spec) :into specs
            :finally (return (values names specs)))
    (setf path (ensure-list path))
    (let ((nowrap (when (eq (car body) :nowrap) (pop body) t)))
      `(progn ,@(loop :for context :in (ensure-list contexts)
                      :collect `(defservice* ',(ensure-list methods) ',context ',path
                                             ',arg-specs ,(make-uri-lambda path arg-names body)
                                             ,nowrap))))))

(defun defservice* (methods context path arg-specs func nowrap)
  (let ((uri (find-uri context path))
        (handler (make-handler :args arg-specs :func func :nowrap nowrap)))
    (dolist (method methods)
      (setf (aref (uri-methods uri) (or (method-pos method) (error "Unsupported method"))) handler))))

(defmacro defcontext (name context path &body body)
  (setf path (ensure-list path))
  (let ((cont (gensym)))
    `(let ((wrap ,(and body (make-uri-lambda path (list 'uri* cont)
                              `((declare (ignorable uri*))
                                (flet ((go-on () (funcall ,cont)))
                                  (declare (ignorable #'go-on))
                                  ,@body))))))
       ,@(loop :for cnt :in (ensure-list context) :collect
           `(defcontext* ',name ',cnt ',path wrap)))))

(defun defcontext* (name context path wrap)
  (multiple-value-bind (uri edge) (find-uri context path (gethash name *contexts*))
    (when wrap
      (etypecase edge
        (split (dolist (path (split-paths edge))
                 (when (eq uri (path-uri path)) (setf (path-wrap path) wrap))))
        (named-resource (setf (named-resource-wrap edge) wrap))))
    (setf (gethash name *contexts*) uri)))

(defmacro allow-empty-resource-name (context path)
  `(progn ,@(loop :for cnt :in (ensure-list context) :collect
               `(allow-empty-resource-name* ',cnt ',(ensure-list path)))))

(defun allow-empty-resource-name* (context path)
  (let ((edge (nth-value 1 (find-uri context path))))
    (check-type edge named-resource)
    (setf (named-resource-allow-empty edge) t)))

(defun find-uri (context path &optional existing)
  (let* ((uri (get-context context))
         (edge nil))
    (loop :for (element . rest) :on path
          :for ex := (if rest nil existing)
          :do (multiple-value-setq (uri edge)
                (etypecase element
                  (string (extend-split uri element ex))
                  (symbol (if (eql (char (symbol-name element) (1- (length (symbol-name element)))) #\*)
                              (extend-tail uri ex)
                              (extend-named uri nil ex))))))
    (values uri edge)))

(defun verify-type (uri type)
  (cond ((or (not uri) (typep uri type)) uri)
        (t (warn "Conflicting use of uri part.")
           nil)))

(defun extend-split (uri name &optional newuri)
  (let ((split (or (verify-type (uri-continue uri) 'split)
                   (setf (uri-continue uri) (make-split)))))
    (let ((found (find name (split-paths split) :test #'string= :key #'path-name)))
      (cond
        (found (assert (or (not newuri) (eq (path-uri found) newuri)))
               (setf newuri (path-uri found)))
        (t (unless newuri (setf newuri (make-uri)))
           (setf (split-paths split) (nconc (split-paths split) (list (make-path :name name :uri newuri))))))
      (values newuri split))))

(defun extend-named (uri &optional wrap newuri)
  (let ((cont (verify-type (uri-continue uri) 'named-resource)))
    (cond (cont (assert (or (not newuri) (eq (named-resource-uri cont) newuri)))
                (values (named-resource-uri cont) cont))
          (t (unless newuri (setf newuri (make-uri)))
             (setf (uri-continue uri) (make-named-resource :wrap wrap :uri newuri))
             (values newuri (uri-continue uri))))))

(defun extend-tail (uri &optional newuri)
  (let ((cont (verify-type (uri-continue uri) 'tail)))
    (cond (cont (assert (or (not newuri) (eq (tail-uri cont) newuri)))
                (values (tail-uri cont) cont))
          (t (unless newuri (setf newuri (make-uri)))
             (setf (uri-continue uri) (make-tail :uri newuri))
             (values newuri (uri-continue uri))))))

(defmacro nlet (name (&rest arguments) &body body)
  `(labels ((,name ,(mapcar #'car arguments) ,@body))
     (,name ,@(mapcar #'cadr arguments))))

(defun split-uri (uri)
  (loop :for start := 1 :then (1+ next) :for next := (position #\/ uri :start start)
        :collect (subseq uri start next) :into parts
        :do (unless next
              (return (if (equal parts '("")) () (mapcar #'url-decode parts))))))

(defun dispatch-request (start-context method raw-uri param-reader)
  (let ((method-pos (or (method-pos method) (method-not-allowed)))
        (parts (split-uri raw-uri)))
    (nlet part ((left (split-uri raw-uri))
                (uri (get-context start-context))
                (names ()))
      (if left
          (let ((elt (car left))
                (continue (uri-continue uri))
                next-uri wrap)
            (when (equal elt "")
              (unless (and (typep continue 'named-resource) (named-resource-allow-empty continue))
                (moved-permanently (with-output-to-string (out)
                                     (loop :for cons :on parts :do
                                        (unless (eq cons left)
                                          (format out "/~a" (car cons))))))))
            (etypecase continue
              (named-resource
               (push elt names)
               (setf next-uri (named-resource-uri continue) wrap (named-resource-wrap continue)))
              (split
               (let ((found (or (find elt (split-paths continue) :test #'string= :key #'path-name)
                                (not-found "'~a' (up from '~a') not found." raw-uri elt))))
                 (setf next-uri (path-uri found) wrap (path-wrap found))))
              (tail
               (let ((handler (or (aref (uri-methods (tail-uri continue)) method-pos) (method-not-allowed))))
                 (return-from part
                   (apply (handler-func handler) (cons left names)
                          (loop :for arg :in (handler-args handler) :collect (apply param-reader arg))))))
              (null (not-found "'~a' (up from '~a') not found." raw-uri elt)))
            (if (and wrap (or (cdr left)
                              (let ((handler (aref (uri-methods next-uri) method-pos)))
                                (not (and handler (handler-nowrap handler))))))
                (funcall wrap names (cdr left) (lambda () (part (cdr left) next-uri names)))
                (part (cdr left) next-uri names)))
          (let ((handler (or (aref (uri-methods uri) method-pos)
                             (if (some #'identity (uri-methods uri))
                                 (method-not-allowed)
                                 (not-found "'~a' does not identify a resource (but its descendants do)." raw-uri)))))
            (apply (handler-func handler) names
                   (loop :for arg :in (handler-args handler) :collect (apply param-reader arg))))))))
