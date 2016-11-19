;;;; utils.lisp

(in-package #:githappy)

(defun url-encode (string)
  (drakma:url-encode string :utf-8))

(defun utf8-string (octets)
  (trivial-utf-8:utf-8-bytes-to-string octets))

(defun jref (json key)
  "Extract KEY from JSON. If KEY is an integer, use ELT; if it's a
  string, use GETHASH; if it's a list, call jref recursively on the
  first element."
  (etypecase key
    (string
     (gethash key json))
    (integer
     (elt json key))
    (null
     json)
    (cons
     (let ((sub-key (first key))
           (rest (rest key)))
       (if (eql sub-key :*)
           (mapcar (lambda (sub-json)
                     (jref sub-json rest))
                   json)
           (jref (jref json (first key)) (rest key)))))))

(defun jdump (json &optional (stream *standard-output*))
  "Write JSON formatting of JSON object to STREAM."
  (yason:encode json (yason:make-json-output-stream stream
                                                    :indent t)))

(defun jdump-to-string (json)
  (with-output-to-string (s) (jdump json s)))

(defun table (&rest keys-and-values)
  "Make a table for feeding to YASON:ENCODE."
  (flet ((stringize (thing)
           (etypecase thing
             (string thing)
             (keyword (string-downcase thing)))))
    (let ((table (make-hash-table :test 'equal)))
      (loop for (key value) on keys-and-values by #'cddr
            do (setf (gethash (stringize key) table) value))
      table)))

(defun js (&rest keys-and-values)
  (with-output-to-string (s)
    (yason:encode (apply #'table keys-and-values) s)))

(defun split (delimiter string)
  (ppcre:split (ppcre:quote-meta-chars delimiter) string))

(defun starts-with (subseq seq &key (test 'eql))
  (and (<= (length subseq)
           (length seq))
       (= (mismatch subseq seq :test test)
          (length subseq))))

(defun sleep-until (universal-time)
  (let ((delta (- universal-time (get-universal-time))))
    (when (plusp delta)
      (sleep delta))))

(defparameter *github-url-patterns*
  '(("github.com/(.*?)/(.*?)(\\.git|/)?$" :owner :repo)
    ("@github.com:(.*?)/(.*?)\\.git$" :owner :repo)))

(defun parse-github-url (url)
  (dolist (pattern *github-url-patterns*)
    (destructuring-bind (regex &rest bindings)
        pattern
      (multiple-value-bind (start end starts ends)
          (ppcre:scan regex url)
        (declare (ignore start end))
        (when (<= (length bindings) (length starts))
          (return
            (loop for binding in bindings
                  for start across starts
                  for end across ends
                  collect binding
                  collect (subseq url start end))))))))
