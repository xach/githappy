;;;; http-protocols.lisp

(in-package #:githappy)

;;; Common to requests & responses
(defgeneric body (http-message))
(defgeneric (setf body) (new-value http-message))
(defgeneric body-string (http-message))
(defgeneric uri (http-message))
(defgeneric (setf uri) (new-value http-message))

;;; Headers
(defgeneric headers (http-message)
  (:documentation "Return the headers of HTTP-MESSAGE as an alist."))
(defgeneric header-value (name http-message))
(defgeneric (setf header-value) (new-value name request))
(defgeneric header-value-string (value)
  (:method (value)
    (princ-to-string value)))
(defgeneric add-header (name value request))
(defgeneric ensure-header (name value request))
(defgeneric call-for-headers (http-message fun)
  (:documentation "For each header in REQUEST, call FUN with two
  arguments: the header name and the header value."))

;;; Requests
(defgeneric submit (request))
(defgeneric response-class (request))
(defgeneric find-http-error-for (request))
(defgeneric check-error (request)
  (:method (request)
    (let ((error (find-http-error-for request)))
      (when error
        (error error :request request)))))
(defgeneric request-method (request))
(defgeneric (setf request-mehtod) (new-value request))

;;; Parameters
(defgeneric request-parameters (request)
  (:documentation "Return the HTTP parameters of REQUEST as an
  alist. The alist should not be mutated."))

(defgeneric (setf request-parameters) (new-value request)
  (:documentation "Set the parameters of REQUEST to the alist
  NEW-VALUE."))

(defgeneric parameter-value-string (value)
  (:documentation "Convert a parameter value to a string.")
  (:method (value)
    (princ-to-string value)))

(defgeneric parameter-value (name request)
  (:documentation "Get the parameter value identified by NAME in
  REQUEST."))

(defgeneric (setf parameter-value) (new-value name request)
  (:documentation "Set the value of the parameter identified by NAME
  to NEW-VALUE. NEW-VALUE is converted to a string with
  PARAMETER-VALUE-STRING."))

(defgeneric add-parameter (name value request)
  (:documentation "Add a new parameter identified by NAME and VALUE to
  REQUEST. VALUE is converted to a string with
  PARAMETER-VALUE-STRING."))

(defgeneric ensure-parameter (name value request)
  (:documentation "Add a new parameter identified by NAME and VALUE to
  REQUEST if there is not already a parameter by that name.")
  (:method (name value request)
    (unless (parameter-value name request)
      (add-parameter name value request))))

(defgeneric call-for-parameters (request fun)
  (:documentation "For each parameter in REQUEST, call FUN with two arguments: the parameter name, and the parameter value."))

;;; Responses
(defgeneric status-code (response))
(defgeneric reason-phrase (response))
(defgeneric request (response)
  (:documentation "What request was used to generate this response?"))

(defgeneric process-response (response)
  (:documentation "Perform any additional processing of RESPONSE
  needed after receiving it. Returns the response object.")
  (:method (response)
    response))
