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
     (jref (jref json (first key)) (rest key)))))

(defun jdump (json &optional (stream *standard-output*))
  "Write JSON formatting of JSON object to STREAM."
  (yason:encode json (yason:make-json-output-stream stream
                                                    :indent t)))

(defun jdump-to-string (json)
  (with-output-to-string (s) (jdump json s)))

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
