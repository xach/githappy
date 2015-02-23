;;;; github-protocols.lisp

(in-package #:githappy)

;;; Github requests

(defgeneric json (response))

;;; Higher-level interface

(defgeneric name (object))

;;; Repos

(defgeneric owner (repo))
(defgeneric participation-stats (repo))
(defgeneric stargazers (repo))
(defgeneric contributors (repo))
(defgeneric languages (repo))
