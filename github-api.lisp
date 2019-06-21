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

(defmacro define-api-request (name route &key documentation)
  "Define an API request function for the given ROUTE, which must be a
string or a list of (HTTP-METHOD ROUTE &REST PARAMETERS). BODY is
evaluated to produce a JSON value for the body of the request. ROUTE
looks like /foo/:bar/baz to produce a keyword of BAR in the resulting
defun, which is filled into the request path. PARAMETERS is a list of
extra parameters that are converted to keyword arguments; if a
parameter is a list, the first element is the keyword name and the
second should be a string that is used as the 'real' HTTP parameter
name, e.g. (public-name \"publicName\")."
  (labels ((normal-symbolize (keyword)
             (intern (symbol-name keyword)))
           (parameter-symbol (parameter)
             (if (consp parameter)
                 (first parameter)
                 parameter))
           (parameter-string (parameter)
             (if (consp parameter)
                 (second parameter)
                 (string-downcase parameter))))
    (let* ((real-route (if (consp route) (second route) route))
           (http-method (if (consp route) (first route) :get))
           (parameters (if (consp route) (cddr route) nil))
           (route-parts (parse-git-api-route real-route))
           (route-keywords (mapcar #'normal-symbolize
                                   (remove-if-not #'keywordp route-parts)))
           (keywords (append (mapcar #'parameter-symbol parameters)
                             route-keywords))
           (lambda-list (list* '&key 'body 'page 'per-page keywords))
           (format-control-string (route-parts-format-control route-parts)))
      `(defun ,name ,lambda-list
         ,@(when documentation
                 (list documentation))
         (let ((request (make-instance 'github-request
                                       :body body
                                       :request-method ',http-method
                                       :uri (format nil ,format-control-string
                                                    ,@route-keywords))))
           (when page
             (ensure-parameter "page" page request))
           (when per-page
             (ensure-parameter "per_page" per-page request))
           ,@(loop for parameter in parameters
                   collect `(when ,(parameter-symbol parameter)
                              (ensure-parameter ,(parameter-string parameter)
                                                ,(parameter-symbol parameter)
                                                request)))
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
(define-api-request repo-issue "/repos/:owner/:repo/issues/:number")
(define-api-request modify-repo-issue (:patch
                                       "/repos/:owner/:repo/issues/:number"))
(define-api-request create-repo-issue (:post
                                       "/repos/:owner/:repo/issues"))

(define-api-request create-repo-issue-comment
  (:post "/repos/:owner/:repo/issues/:issue-number/comments"))

(define-api-request user-gists (:get "/users/:username/gists" since)
  :documentation "Return public gists for the specified user.")

(define-api-request gists (:get "/gists" since)
  :documentation "If authenticated, return authenticated user's gists,
  otherwise return all public gists.")

(define-api-request public-gists (:get "/gists/public" since)
  :documentation "Return all public gists.")

(define-api-request starred-gists (:get "/gists/starred" since))

(define-api-request gist-info "/gists/:id")
(define-api-request gist-revision-info "/gists/:id/:sha")
(define-api-request gist-commits "/gists/:id/commits")
(define-api-request star-gist (:put "/gists/:id/star"))
(define-api-request unstar-gist (:delete "/gists/:id/star"))
;; FIXME: gitst-start-status can signal an error due to a 404, but 404
;; is part of the declared API - should return the 404 response object
;; instead. Generalize macro to handle certain non-2xx responses in
;; certain contexts?
(define-api-request gist-star-status "/gists/:id/star")

(define-api-request fork-gist (:post "/gists/:id/forks"))
(define-api-request gist-forks "/gists/:id/forks")
(define-api-request delete-gist (:delete "/gists/:id"))

(define-api-request create-gist (:post "/gists"))
(define-api-request modify-gist (:patch "/gists/:id"))




;;; TODO: Process a github response to extract the prev, next, last
;;; header links, use that to make relative requests,
;;; e.g. (next-response <response-object>), (prev-response
;;; <response-object>), etc.
