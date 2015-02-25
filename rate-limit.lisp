;;;; rate-limit.lisp

(in-package #:githappy)

(defun rate-limit-info (response)
  (flet ((ivalue (name)
           (let ((value (header-value name response)))
             (if value
                 (parse-integer value)
                 0))))
    (list :limit (ivalue "x-ratelimit-limit")
          :remaining (ivalue  "x-ratelimit-remaining")
          :reset (ivalue "x-ratelimit-reset"))))

(defparameter *unix-time-offset*
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun unix-to-universal-time (unix-time)
  (+ *unix-time-offset* unix-time))

(defun rate-limit-time (response)
  (destructuring-bind (&key reset &allow-other-keys)
      (rate-limit-info response)
    (unix-to-universal-time reset)))

(define-api-request rate-limit "/rate_limit")

(defun rate-limit-sleep ()
  "Assuming constant activity ")

(defun rate-limit-budget (&optional response)
  (let ((response (or response (rate-limit))))
    (destructuring-bind (&key limit remaining reset)
        (rate-limit-info response)
      (let ((now (get-universal-time))
            (end (unix-to-universal-time reset)))
        (list :remaining remaining
              :time (- end now))))))
