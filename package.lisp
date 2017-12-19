;;;; package.lisp

(defpackage #:githappy
  (:use #:cl)
  (:export #:json
           #:jref
           #:=jref
           #:js
           #:table
           #:*github-url-patterns*
           #:parse-github-url
           #:user-repos
           #:org-repos
           #:repo-info
           #:repo-participation-stats
           #:repo-stargazers
           #:repo-contributors
           #:repo-languages
           #:repo-teams
           #:repo-tags
           #:repo-branches
           #:repo-branch-info
           #:repo-releases
           #:repo-issues
           #:repo-issue
           #:modify-repo-issue
           #:user-gists
           #:gists
           #:public-gists
           #:starred-gists
           #:gist-revision-info
           #:gist-commits
           #:star-gist
           #:unstar-gist
           #:gist-star-status
           #:fork-gist
           #:gist-forks
           #:delete-gist
           #:create-gist
           #:modify-gist
           #:*oauth2-token*))

