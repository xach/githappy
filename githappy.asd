;;;; githappy.asd

(asdf:defsystem #:githappy
  :description "githappy is for accessing the GitHub REST API."
  :author "Zach Beane <xach@xach.com>"
  :license "MIT"
  :depends-on (#:cl-ppcre
               #:drakma
               #:trivial-utf-8
               #:cl-ppcre
               #:yason)
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "http-protocols")
               (:file "http")
               (:file "github-api")
               (:file "github-protocols")
               (:file "github")
               (:file "rate-limit")))

