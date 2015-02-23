;;;; http.lisp

(in-package #:githappy)

(defclass http-message ()
  ((uri
    :initarg :uri
    :accessor uri)
   (headers
    :initarg :headers
    :accessor headers)
   (body
    :initarg :body
    :accessor body))
  (:default-initargs
   :body nil
   :headers nil))

(defclass request (http-message)
  ((request-method
    :initarg :request-method
    :accessor request-method)
   (request-parameters
    :initarg :request-parameters
    :accessor request-parameters)
   (response-class
    :initarg :response-class
    :initform 'response
    :reader response-class))
  (:default-initargs
   :request-method :get
   :request-parameters nil))

(defmethod print-object ((request request) stream)
  (print-unreadable-object (request stream :type t)
    (format stream "~S to ~S"
            (request-method request)
            (uri request))))

(defmethod parameter-value (name (request request))
  (cdr (assoc name (request-parameters request) :test 'string=)))

(defmethod add-parameter (name value (request request))
  (push (cons name (parameter-value-string value))
        (request-parameters request)))

(defmethod call-for-parameters ((request request) fun)
  (loop for (name . value) in (request-parameters request)
        do (funcall fun name value)))



(defmethod submit ((request request))
  (multiple-value-bind (body status-code headers uri
                             stream must-close-p reason-phrase)
      (drakma:http-request (uri request)
                           :method (request-method request)
                           :parameters (request-parameters request)
                           :additional-headers (headers request)
                           :want-stream nil)
    (when must-close-p
      (close stream))
    (process-response (make-instance (response-class request)
                                     :uri uri
                                     :body body
                                     :status-code status-code
                                     :reason-phrase reason-phrase
                                     :headers headers
                                     :request request))))

(defclass response (http-message)
  ((status-code
    :initarg :status-code
    :reader status-code)
   (reason-phrase
    :initarg :reason-phrase
    :reader reason-phrase)
   (request
    :initarg :request
    :reader request))
  (:default-initargs
   :status-code 0
   :reason-phrase "[not initialized]"
   :uri "[not initialized]"
   :request nil))

(defmethod header-value (name http-message)
  (cdr (assoc name (headers http-message) :test 'string-equal)))


