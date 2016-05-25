;;;; api.lisp

(in-package #:githappy)

;;; Low-level API interface

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-git-api-route (route)
    (let ((parts (split "/" route)))
      (labels ((variablep (part)
                 (and (starts-with ":" part)))
               (keywordize (part)
                 (intern (string-upcase (subseq part 1))
                         :keyword))
               (process (part)
                 (if (variablep part)
                     (keywordize part)
                     part)))
        (mapcar #'process parts))))

  (defun route-parts-format-control (route-parts)
    (format nil "https://api.github.com~{~A~^/~}"
            (substitute-if "~A" #'keywordp route-parts))))

(defmacro define-api-request (name route)
  "Define an API request function for the given ROUTE, which must be a
string or a list of (HTTP-METHOD ROUTE). BODY is evaluated to produce
a JSON value for the body of the request."
  (labels ((normal-symbolize (keyword)
             (intern (symbol-name keyword))))
    (let* ((real-route (if (consp route) (second route) route))
           (http-method (if (consp route) (first route) :get))
           (route-parts (parse-git-api-route real-route))
           (keywords (mapcar #'normal-symbolize
                             (remove-if-not #'keywordp route-parts)))
           (lambda-list (list* '&key 'body 'page 'per-page keywords))
           (format-control-string (route-parts-format-control route-parts)))
      `(defun ,name ,lambda-list
         (let ((request (make-instance 'github-request
                                       :body body
                                       :request-method ',http-method
                                       :uri (format nil ,format-control-string
                                                    ,@keywords))))
           (when page
             (ensure-parameter "page" page request))
           (when per-page
             (ensure-parameter "per_page" per-page request))
           (submit request))))))

(define-api-request user-repos "/users/:username/repos")
(define-api-request org-repos "/orgs/:org/repos")
(define-api-request repo-info "/repos/:owner/:repo")
(define-api-request repo-participation-stats
  "/repos/:owner/:repo/stats/participation")
(define-api-request repo-stargazers "/repos/:owner/:repo/stargazers")
(define-api-request repo-contributors "/repos/:owner/:repo/contributors")
(define-api-request repo-languages "/repos/:owner/:repo/languages")
(define-api-request repo-teams "/repos/:owner/:repo/teams")
(define-api-request repo-tags "/repos/:owner/:repo/tags")
(define-api-request repo-branches "/repos/:owner/:repo/branches")
(define-api-request repo-branch-info "/repos/:owner/:repo/branches/:branch")
(define-api-request repo-releases "/repos/:owner/:repo/releases")
(define-api-request repo-issues "/repos/:owner/:repo/issues")

(define-api-request modify-repo-issue (:patch
                                       "/repos/:owner/:repo/issues/:number"))



;;; TODO: Process a github response to extract the prev, next, last
;;; header links, use that to make relative requests,
;;; e.g. (next-response <response-object>), (prev-response
;;; <response-object>), etc.
