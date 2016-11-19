;;;; github.lisp

(in-package #:githappy)

(defvar *oauth2-token* nil
  "An OAuth2 token to use for authenticated requests. When this
  variable is set to a valid token, the rate limit goes from 60
  requests per hour to 5000 (as of 2015-02-25). Tokens can be obtained
  via a web authentication process. Personal access tokens can be
  generated via https://github.com/settings/applications, which is
  much easier than going through the web auth process.")

(defclass github-response (response)
  ((json
    :initarg :json
    :reader json
    :writer (setf %json)))
  (:default-initargs
   :json nil))

(defclass github-request (request)
  ()
  (:default-initargs
   :response-class 'github-response))

(defmethod submit ((request github-request))
  (when *oauth2-token*
    (ensure-header "Authorization"
                   (format nil "token ~A" *oauth2-token*)
                   request))
  (let ((delay 1)
        (attempt 0)
        (max 8))
    (loop
      (when (<= max (incf attempt))
        (error "Timeout"))
      (setf delay (* delay 2))
      (let* ((response (call-next-method))
             (code (status-code response)))
        (unless (eql code 202)
          (return response))
        (warn "Retrying on 202 response")
        (sleep delay)))))


(define-condition github-response-error (response-error)
  ((message
    :initarg :response
    :reader github-response-error-response))
  (:report
   (lambda (condition stream)
     (let ((response (github-response-error-response condition)))
       (format stream "~A ~S from ~A~%"
               (status-code response)
               (reason-phrase response)
               (request response))))))

(defmethod process-response ((response github-response))
  (unless (stringp (body response))
    (setf (body response)
          (utf8-string (body response))))
  (when (search "application/json" (header-value "content-type" response))
    (setf (%json response) (yason:parse (body response))))
  (unless (<= 200 (status-code response) 299)
    (error 'github-response-error
           :response response))
  response)



;;; TODO: Rate limiting
