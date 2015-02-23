;;;; github.lisp

(in-package #:githappy)

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

(defmethod process-response ((response github-response))
  (unless (stringp (body response))
    (setf (body response)
          (utf8-string (body response))))
  (when (search "application/json" (header-value "content-type" response))
    (setf (%json response) (yason:parse (body response))))
  response)



