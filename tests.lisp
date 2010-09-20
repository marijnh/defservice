(defpackage :defservice.tests
  (:use :defservice :cl))

(in-package :defservice.tests)

(define-condition failure (simple-error) ())
(defun fail (control &rest args)
  (error 'failure :format-control control :format-arguments args))

(defmacro $assert (a)
  `(unless ,a
     (fail "assert(~s) failed" ',a)))
(defmacro $equal (a b)
  (let ((a1 (gensym)) (b1 (gensym)))
    `(let ((,a1 ,a) (,b1 ,b))
       (unless (equal ,a1 ,b1)
         (fail "equal(~s, ~s) failed: equal(~s, ~s)" ',a ',b ,a1 ,b1)))))
(defmacro $code (code &body body)
  `(handler-case (progn ,@body (fail "No condition raised."))
     (dispatch-failed (e) (unless (= (dispatch-failed-code e) ,code)
                            (fail "code ~a instead of ~a raised." (dispatch-failed-code e) ,code)))))

(defun run-tests (&optional (package (find-package :defservice.tests)))
  (let ((failures ()) (run 0))
    (do-symbols (sym package)
      (when (and (eq (symbol-package sym) package)
                 (> (length (symbol-name sym)) 2)
                 (equal (subseq (symbol-name sym) 0 2) "t-"))
        (incf run)
        (handler-case (funcall sym)
          (failure (e) (push (format nil "~a: ~a" sym e) failures))
          (error (e) (push (format nil "~a ERROR: ~a" sym e) failures)))))
    (if failures
        (format t "!! ~a failures:~%~{  ~a~%~}" (length failures) (nreverse failures))
        (format t "All passed (~a tests run).~%" run))))

(defvar *params* ())
(defun read-param (name type &optional default)
  (declare (ignore type))
  (let ((found (assoc name *params* :test #'string=)))
    (if found
        (cdr found)
        default)))
(defmacro with-params ((&rest params) &body body)
  `(let ((*params* (append (list ,@(loop :for (n v) :on params :by #'cddr :collect `(cons ,n ,v))) *params*)))
     ,@body))

(defun dispatch (method url)
  (dispatch-request 'test method url 'read-param))

(make-start-context 'test)

(defservice :get test ("foo" "bar") () 'foobar)

(defun t-basic ()
  ($equal (dispatch :get "/foo/bar") 'foobar))

(defservice :get test "params" ((x :tx) (y :ty :dy) ((z "zName") :tz :dz))
  (list x y z))

(defun t-params ()
  (with-params ("x" :xx "z" :zz)
    ($equal (dispatch :get "/params") '(:xx :dy :dz))
    (with-params ("zName" :zz)
      ($equal (dispatch :get "/params") '(:xx :dy :zz)))))

(defservice :get test () () 'root)

(defun t-root ()
  ($equal (dispatch :get "/") 'root))

(defservice :get test "methods" () 'get)
(defservice (:put :post) test "methods" () 'put/post)
(defservice :delete test "methods" () 'delete)

(defun t-methods ()
  ($equal (dispatch :get "/methods") 'get)
  ($equal (dispatch :put "/methods") 'put/post)
  ($equal (dispatch :post "/methods") 'put/post)
  ($equal (dispatch :delete "/methods") 'delete)
  ($equal (dispatch :head "/methods") 'get))

(defservice :get test ("named" name) () name)

(defun t-named ()
  ($equal (dispatch :get "/named/blah") "blah")
  ($equal (dispatch :get "/named/%20") " ")
  ($equal (dispatch :get "/n%61med/u") "u"))

(defvar *special* nil)

(defcontext spec test ("special" name) (let ((*special* name)) (go-on)))

(defservice :get spec () () *special*)
(defservice :put spec () () :nowrap *special*)

(defun t-context ()
  ($equal (dispatch :get "/special/hi") "hi")
  ($equal (dispatch :put "/special/hi") nil))

(defun t-not-found ()
  ($code 404 (dispatch :get "/does/not/exist"))
  ($code 405 (dispatch :put "/foo/bar"))
  ($code 301 (dispatch :get "/foo/bar/")))

(defcontext join test ("through" name "to") (let ((*special* name)) (go-on)))
(defcontext join test ("to"))
(defservice :get join name () (cons *special* name))

(defun t-join ()
  ($equal (dispatch :get "/through/a/to/b") '("a" . "b"))
  ($equal (dispatch :get "/to/c") '(nil . "c")))

(defservice :get test ("tail" rest*) ()
  rest*)

(defun t-tail ()
  ($equal (dispatch :get "/tail/foo/bar") '("foo" "bar"))
  ($equal (dispatch :get "/tail/foo/bar/") '("foo" "bar" "")))

(defservice :get test ("notempty" foo) () foo)
(defservice :get test ("empty" foo) () foo)
(allow-empty-resource-name test ("empty" foo))

(defun t-empty ()
  ($code 301 (dispatch :get "/notempty/"))
  ($equal (dispatch :get "/empty/") ""))
