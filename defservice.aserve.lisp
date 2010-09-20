#-allegro(error "defservice.aserve is (currently) Allegro-specific code. (Porting shouldn't be hard.)")

(eval-when (eval load compile) (require :aserve))

(defpackage :defservice.aserve
  (:use :defservice :cl :excl :net.aserve)
  (:export #:aserve-param-reader))

(in-package :defservice.aserve)

(defun aserve-param-reader (request)
  (lambda (name type &optional (default nil default-p))
    (case type
      (:body (or (get-request-body request :external-format :utf-8) ""))
      (:body-binary (string-to-octets (get-request-body request) :null-terminate nil :external-format :octets))
      (:content-type (let* ((ct (header-slot-value request :content-type))
                            (sc (position #\; ct)))
                       (if sc (subseq ct 0 sc) ct)))
      (:request request)
      (:method (request-method request))
      (:list (loop :for (nm . val) :in (request-query request :external-format :utf-8)
                   :when (string= nm name) :collect val))
      (:vars (loop :for param :in (request-query request :external-format :utf-8)
                   :when (eql (char (car param) 0) #\$) :collect param))
      (t (let ((str (request-query-value name request :external-format :utf-8)))
           (cond (str
                  (multiple-value-bind (value ok) (read-parameter-type type str)
                    (unless ok
                      (error 'dispatch-failed :status 400
                             :message (format nil "'~a' is not a valid ~a." str type)))
                    value))
                 (default-p default)
                 (t (error 'dispatch-failed :status 400
                           :message (format nil "No value given for parameter '~a'." name)))))))))

(defgeneric read-parameter-type (type string)
  (:method ((type (eql :string)) string)
    (values string t))
  (:method ((type (eql :boolean)) string)
    (if (string= string "true") (values t t) (values nil t)))
  (:method ((type (eql :integer)) string)
    (handler-case (values (parse-integer string) t)
      (error () nil))))
