;;; repology.el --- Repology API access via Elisp    -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <mail@nicolasgoaziou.fr>
;; Maintainer: Nicolas Goaziou <mail@nicolasgoaziou.fr>
;; Keywords: web
;; Package-Requires: ((emacs "26.1"))
;; Version: 0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides tools to query Repology API
;; (<https://repology.org/api>), process results, and display them.

;; The results of a query revolve around three types of objects:
;; projects, packages and problems.  Using this library, you can find
;; projects matching certain criteria, packages in a given project,
;; and possible problems in some repository.  See `repology-search-projects',
;; `repology-lookup-project', and `repology-report-problems'.
;; Projects-related requests are limited to `repology-projects-limit'.
;; All requests are cached during `repology-cache-duration' seconds.
;;
;; By default, only projects recognized as free are included in the search
;; results.  You can control this behavior with the variable
;; `repology-free-only-projects'.  The function `repology-free-p' is responsible
;; for guessing if a project, or a package, is free or not.

;; You can then access data from those various objects using dedicated
;; accessors.  See, for example, `repology-project-name',
;; `repology-project-packages', `repology-package-field', or
;; `repology-problem-field'.

;; You can also decide to display (a subset of) results in a tabulated
;; list.  See `repology-display-package', `repology-display-packages',
;; `repology-display-projects' and `repology-display-problems'.  You
;; can control various aspects of the display, like the colors used
;; (see `repology-status-faces'), or the columns shown (see
;; `repology-display-packages-columns',`repology-display-projects-columns',
;; and `repology-display-problems-columns').  When projects or packages
;; are displayed, pressing <RET> gives you more information about the item
;; at point, whereas pressing <F> reports their "freedom" status.

;; For example, the following expression displays all outdated projects
;; named after "emacs" and containing a package in GNU Guix repository
;; that I do not ignore:
;;
;;    (repology-display-projects
;;     (seq-filter (lambda (project)
;;                   (not (member (repology-project-name project)
;;                                my-ignored-projects)))
;;                 (repology-search-projects
;;                  :search "emacs" :inrepo "gnuguix" :outdated "on")))

;; Eventually, this library provides an interactive function with
;; a spartan interface wrapping this up: `repology'.  Since it builds
;; and displays incrementally search filters, you may use it as
;; a template to create your own queries.

;; Known issues:
;;
;; - The library has no notion of distribution "family", since this
;;   doesn't appear in the API.  As a consequence, display functions
;;   cannot compute the "spread" of a project.  It falls back to the
;;   number of packages in the project instead.
;; - It does not handle "maintainers" queries.
;; - It is synchronous.  Don't go wild with `repology-projects-limit'!

;;; Code:

(require 'json)
(require 'tabulated-list)
(require 'url)

(require 'repology-license)


;;; Macros
;; XXX: It is a macro because we need it to be available in defcustoms.
(defmacro repology-display-sort-column (name predicate)
  "Return a function comparing entries in column NAME.
NAME is a string.  Compare entries using function PREDICATE, called on two
objects of the column."
  `(lambda (e1 e2)
     (let ((column
            ;; Find column's number
            (or (seq-position tabulated-list-format
                              ,name
                              (pcase-lambda (`(,n . ,_) s) (equal n s)))
                (error "Invalid column name %S" ,name))))
       (let ((s1 (elt (cadr e1) column))
             (s2 (elt (cadr e2) column)))
         (funcall ,predicate s1 s2)))))


;;; Upstream Constants
(defconst repology-base-url "https://repology.org/api/v1/"
  "Base URL for Repology API.")

(defconst repology-statistics-url "https://repology.org/repositories/statistics"
  "URL for \"Statistics\" page in Repology website.
It is used as a source for all known repositories.")

(defconst repology-package-all-fields
  '(repo subrepo name srcname binname visiblename version origversion status
         summary categories licenses maintainers www downloads)
  "List of known package fields.")

(defconst repology-package-all-status
  '("newest" "devel" "unique" "outdated" "legacy" "rolling" "noscheme"
    "incorrect" "untrusted" "ignored")
  "List of known status values.")

(defconst repology-projects-hard-limit 200
  "Maximum number of projects Repology API can return.
See URL `https://repology.org/api'.")


;;; Configuration
(defgroup repology nil
  "Repology API access from Emacs"
  :group 'emacs)

(defcustom repology-projects-limit 200
  "Maximum number of results for a single projects search.

One request to Repology API can return at most `repology-projects-hard-limit'
projects.  Setting the variable to a value greater than this implies the library
will sent multiple requests upstream to collect the desired number of results."
  :type 'integer)

(defcustom repology-cache-duration 3600
  "Duration in seconds to cache Repology API requests.

Repology claims to update its repository hourly.
A value of 0 prevents any caching."
  :type 'integer)

(defcustom repology-free-only-projects t
  "When non-nil, return only free projects from searches.

Declaring a project as free the consequence of a very conservative process.
Free projects with missing licensing information, or too confidential, may be
ignored.  See `repology-free-p' for more information."
  :type 'boolean)

(defcustom repology-status-faces
  '(("incorrect" . error)
    ("newest" . highlight)
    ("outdated" . warning)
    ("noscheme" . shadow)
    ("untrusted" . shadow)
    ("ignored" . shadow))
  "Association list of status values and faces.

Each entry is a construct like (STATUS . FACE) where STATUS is
a possible package status value, as detailed in `repology-package-field',
and FACE is the face to be applied by `repology-package-colorize-status'
and `repology-package-colorize-version'.

Un-handled status values are associated to the `default' face."
  :type
  `(repeat
    (cons :tag "Association"
          (choice :tag "Status"
                  ,@(mapcar (lambda (status) `(const ,status))
                            repology-package-all-status))
          face)))

(defcustom repology-display-problems-columns
  `(("Project" effname 20 t)
    ("Package name" visiblename 20 t)
    ("Problem" type 40 t)
    ("Maintainer" maintainers 30 nil))
  "Columns format rules used to display a list of packages.

The value is an association list.  Each entry has the form

  (NAME VALUE WIDTH SORT)

where NAME, WIDTH and SORT are of the expected type in `tabulated-list-format'.
VALUE is either a problem field, as a symbol, or a function called with a single
problem argument.  Its return value is then turned into a string and displayed."
  :type
  '(repeat
    (list :tag "Column definition"
          (string :tag "Column name")
          (choice symbol function)
          (integer :tag "Width")
          (choice (const :tag "Do not sort" nil)
                  (const :tag "Sort" t)
                  (function :tag "Custom sort predicate")))))

(defcustom repology-display-packages-columns
  `(("Repository"
     repology-package-repository-full-name
     20
     ,(repology-display-sort-column "Repository" #'repology-compare-texts))
    ("Name" visiblename 20 t)
    ("Version"
     repology-package-colorized-version
     12
     ,(repology-display-sort-column "Version" #'repology-compare-versions))
    ("Category" categories 25 t)
    ("Maintainer(s)" maintainers 30 t))
  "Columns format rules used to display a list of packages.

The value is an association list.  Each entry has the form

  (NAME VALUE WIDTH SORT)

where NAME, WIDTH and SORT are of the expected type in `tabulated-list-format'.
VALUE is either a valid package field, or a function called with a single
package argument.  Its return value will be turned into a string and displayed.

This library provides a few functions useful as VALUE.  See, for example,
`repology-package-repository-full-name' or `repology-package-colorized-version'.

You may also want to look into `repology-display-sort-column', along with
predicates like `repology-compare-texts', `repology-compare-numbers', or
`repology-compare-versions' in order to build SORT values."
  :type
  `(repeat
    (list :tag "Column definition"
          (string :tag "Column name")
          (choice ,@(mapcar (lambda (field) `(const ,field))
                            repology-package-all-fields)
                  function)
          (integer :tag "Width")
          (choice (const :tag "Do not sort" nil)
                  (const :tag "Sort" t)
                  (function :tag "Custom sort predicate")))))

(defcustom repology-display-projects-columns #'repology-display-projects-default
  "Columns format rules used to display a list of projects.

The value is an association list.  Each entry has the form

  (NAME VALUE WIDTH SORT)

where NAME, WIDTH and SORT are of the expected type in `tabulated-list-format'.
VALUE is a function called with a single package argument.  Its return value
is then turned into a string and displayed.

It can also be a function called with two arguments: the list of projects,
and a selected repository, as a string, or nil.  It must return a list
of the above form.

This library provides a few functions useful as VALUE.  See, for example,
`repology-project-newest-version' or `repology-project-outdated-versions'.

You may also want to look into `repology-display-sort-column', along with
predicates like `repology-compare-texts', `repology-compare-numbers', or
`repology-compare-versions' in order to build SORT values."
  :type '(choice
          (repeat
           (list :tag "Column definition"
                 (string :tag "Column name")
                 function
                 (integer :tag "Width")
                 (choice (const :tag "Do not sort" nil)
                         (const :tag "Sort" t)
                         (function :tag "Custom sort predicate"))))
          (function :tag "Function describing columns")))


;;; Global Internal Variables
(defconst repology-project-filters-parameters
  `((:search          "Name search (e.g. emacs): " nil)
    (:maintainer      "Maintainer (e.g. foo@bar.com): " nil)
    (:category        "Category (e.g. games): " nil)
    (:inrepo          "In repository: " repology--query-repository)
    (:notinrepo       "Not in repository: " repology--query-repository)
    (:repos           "Repositories (e.g. 1 or 2- or 3-5): " nil)
    (:families        "Families (e.g. 1 or 2- or 3-5): " nil)
    (:repos_newest    "Repositories newest (e.g. 1 or 2- or 3-5): " nil)
    (:families_newest "Families newest (e.g. 1 or 2- or 3-5): " nil)
    (:newest          "Newest? " repology--query-y-or-n-p)
    (:outdated        "Outdated? " repology--query-y-or-n-p)
    (:problematic     "Problematic? " repology--query-y-or-n-p)
    (:vulnerable      "Potentially vulnerable? " repology--query-y-or-n-p)
    (:has_related     "Has related? " repology--query-y-or-n-p))
  "Association list between project filters and query data.
Each entry is a triplet (FILTER PROMPT QUERY) where FILTER is a keyword, PROMPT
is a string, and QUERY is a function used to prompt the user, or nil.
When setting the value of FILTER interactively, QUERY is called with
two arguments, PROMPT and an initial value.  It must return a string.  If QUERY
is nil, `read-string' is used.")

(defconst repology--project-filters
  (mapcar #'car repology-project-filters-parameters)
  "List of known filters for projects.
Other keywords are ignored when building the query string.")


;;; Utilities
(defvar repology--repositories nil
  "List of repositories known to Repology.
The list is populated by `repology-list-repositories'.  Call that function
instead of using this variable.")

(defun repology-package-p (object)
  "Return t if OBJECT is a package."
  (and (consp object)
       ;; Mandatory fields.
       (stringp (alist-get 'repo object))
       (stringp (or (alist-get 'name object)
                    (alist-get 'srcname object)
                    (alist-get 'binname object)))
       (stringp (alist-get 'version object))))

(defun repology-project-p (object)
  "Return t if OBJECT is a project."
  (pcase object
    (`(,(pred symbolp) . ,packages)
     (seq-every-p #'repology-package-p packages))
    (_ nil)))

(defun repology-project-name (project)
  "Return PROJECT's name, as a string."
  (unless (repology-project-p project)
    (user-error "No valid project provided"))
  (symbol-name (car project)))

(defun repology-project-packages (project)
  "Return list of packages associated to PROJECT."
  (unless (repology-project-p project)
    (user-error "No valid project provided"))
  (cdr project))

(defun repology-project-newest-version (project)
  "Return newest version string for packages in PROJECT, or nil."
  (let ((newest
         (seq-find (lambda (package)
                     (equal "newest" (repology-package-field package 'status)))
                   (repology-project-packages project))))
    (and newest (repology-package-field newest 'version))))

(defun repology-project-outdated-versions (project)
  "Return a list of outdated versions for packages in PROJECT, or nil.
Versions are sorted in descending order."
  (let ((outdated
         (seq-filter
          (lambda (package)
            (equal "outdated"
                   (repology-package-field package 'status)))
          (repology-project-packages project))))
    (sort (mapcar (lambda (p) (repology-package-field p 'version))
                  outdated)
          ;; Return versions in decreasing order.
          (lambda (s1 s2) (repology-compare-versions s2 s1)))))

(defun repology-package-field (package field)
  "Return PACKAGE's FIELD.

FIELD is a symbol among:

`repo'
    name of repository for this package

`subrepo'
    name of subrepository (if applicable; for example, main or contrib or
    non-free for Debian)

`name', `srcname', `binname'
    package name(s) as used in repository - generic one and/or source package
    name and/or binary package name, whichever is applicable

`visiblename'
    package name as shown to the user by Repology

`version'
    package version (sanitized, as shown by Repology)

`origversion'
    package version as in repository

`status'
    package status, one of \"newest\", \"devel\", \"unique\", \"outdated\", \
\"legacy\",
    \"rolling\", \"noscheme\", \"incorrect\", \"untrusted\", \"ignored\"

`summary'
    one-line description of the package

`categories'
    list of package categories

`licenses'
    list of package licenses

`maintainers'
    list of package maintainers

`www'
    list of package webpages

`downloads'
    list of package downloads

Mandatory fields are `repo', `visiblename', and `version'; all other fields
are optional."
  (unless (memq field repology-package-all-fields)
    (user-error "Unknown field: %S" field))
  (alist-get field package))

(defun repology-package-repository-full-name (package)
  "Return PACKAGE repository's full name.
Return PACKAGE's repository internal name if the full name is unknown."
  (let ((repo (repology-package-field package 'repo)))
    ;; Since `repology-list-repositories' may fail, e.g., due to
    ;; connectivity issues, ensure something is returned anyway, in
    ;; this case, the repository internal name.
    (or (ignore-errors (repology-repository-full-name repo))
        repo)))

(defun repology-package-colorized-status (package)
  "Return colorized status string for PACKAGE.
The version string is emphasized according to PACKAGE's status.
Return nil if PACKAGE has no status field."
  (let ((status (repology-package-field package 'status)))
    (and (stringp status)
         (propertize status 'face (repology--package-status-face package)))))

(defun repology-package-colorized-version (package)
  "Return colorized version string for PACKAGE.
The version string is emphasized according to PACKAGE's status.
See `repology-status-faces'."
  (propertize (repology-package-field package 'version)
              'face
              (repology--package-status-face package)))

(defun repology-problem-field (problem field)
  "Return PROBLEM's FIELD.

FIELD is a symbol.  Repology API does not define an exhaustive list of
allowed symbols.  However, it currently supports, among others, the
following ones:

`repo'
    repository name

`visiblename'
    package name as in Repology

`effname'
    repology project name

`maintainer'
    package maintainer associated with the problem; may be null; note that
    if there are multiple package maintainers, problem is duplicated for
    each one

`type'
    textual description of the problem"
  (alist-get field problem))

(defun repology-list-repositories (&optional full-name)
  "Return repositories known to Repology.

Return a list of strings.  When option argument FULL-NAME is non-nil, list
the repositories with their full name instead of their internal name."
  (unless repology--repositories
    (with-temp-message "Repology: Fetching list of repositories..."
      (let ((request (repology-request repology-statistics-url)))
       (pcase (plist-get request :reason)
         ("OK"
          (let ((body (plist-get request :body))
                (repositories nil)
                (start 0))
            (while (string-match "id=\"\\(.+?\\)\"" body start)
              (setq start (match-end 0))
              (let* ((repo (match-string 1 body))
                     (regexp
                      (rx "href=\"/repository/"
                          (+? anychar)
                          "\">"
                          (group (+? anychar))
                          "<"))
                     (true-name
                      (and (string-match regexp body start)
                           (match-string 1 body))))
                (push (cons repo true-name) repositories)))
            (setq repology--repositories (nreverse repositories))))
         (status
          (error "Cannot retrieve information: %S" status))))))
  (mapcar (if full-name #'cdr #'car) repology--repositories))

(defun repology-refresh-repositories ()
  "Refresh list of repositories known to Repology."
  (setq repology--repositories nil)
  (repology-list-repositories))

(defun repology-repository-name (full-name)
  "Return name of repository named after string FULL-NAME.
Raise an error if FULL-NAME is unknown to Repology."
  (unless (member full-name (repology-list-repositories t))
    (user-error "Unknown repository: %S" full-name))
  (pcase (rassoc full-name repology--repositories)
    (`(,(and (pred stringp) name) . ,_) name)
    (_ (error "Corrupted repository list!"))))

(defun repology-repository-full-name (repository)
  "Return user-facing name for string REPOSITORY.
Raise an error if REPOSITORY is unknown to Repology."
  (unless (member repository (repology-list-repositories))
    (user-error "Unknown repository: %S" repository))
  (or (alist-get repository repology--repositories nil nil #'equal)
      (error "Corrupted repository list!")))

(defun repology-compare-texts (s1 s2)
  "Compare strings S1 and S2 in collation order.
Return t if S1 is less than S2.  Case is ignored."
  (string-collate-lessp s1 s2 nil t))

(defun repology-compare-numbers (s1 s2)
  "Compare strings S1 and S2 numerically.
Return t if S1 is less than S2."
  (< (string-to-number s1) (string-to-number s2)))


;;; Version Comparison
(defconst repology-version-zero-component '(1 . 0)
  "Version component representing 0 or any missing component.")

(defconst repology-version-pre-keywords '("alpha" "beta" "rc" "pre")
  "List of pre-release keywords in version strings.")

(defconst repology-version-post-keywords '("patch" "post" "pl" "errata")
  "List of post-release keywords in version strings.")

(defun repology--string-to-version (s)
  "Return version associated to string S.
Version is a list of components (RANK . VALUE) suitable for comparison, with
the function `repology-compare-versions'."
  (let ((split nil))
    ;; Explode string into numeric and alphabetic components.
    ;; Intermediate SPLIT result is in reverse order.
    (let ((regexp (rx (or (group (one-or-more digit)) (one-or-more alpha))))
          (start 0))
      (while (string-match regexp s start)
        (let ((component (match-string 0 s)))
          (push (if (match-beginning 1) ;numeric component?
                    (string-to-number component)
                  ;; Version comparison ignores case.
                  (downcase component))
                split))
        (setq start (match-end 0))))
    ;; Attach ranks to components.  NUMERIC-FLAG is used to catch
    ;; trailing alphabetic components, which get a special rank.
    ;; However, if there is no numeric component, no alphabetic
    ;; component ever gets this rank, hence the initial value.
    (let ((numeric-flag (seq-every-p #'stringp split))
          (result nil))
      (dolist (component split)
        (let ((rank
               (cond
                ;; 0 gets "zero" (1) rank.
                ((equal 0 component) 1)
                ;; Other numeric components get "nonzero" (3) rank.
                ((wholenump component) 3)
                ;; Pre-release keywords get "pre_release" (0) rank.
                ((member component repology-version-pre-keywords) 0)
                ;; Post-release keywords get "post_release" (2) rank.
                ((member component repology-version-post-keywords) 2)
                ;; Alphabetic components after the last numeric
                ;; component get the "letter_suffix" (4) rank.
                ((not numeric-flag) 4)
                ;; Any other alphabetic component is "pre_release".
                (t 0))))
          (when (wholenump component) (setq numeric-flag t))
          (push (cons rank component) result)))
      result)))

(defun repology-compare-versions (s1 s2)
  "Compare package versions associated to strings S1 and S2.

Return t if version S1 is lower than version S2.

See URL `https://github.com/repology/libversion/blob/master/doc/ALGORITHM.md'."
  (let ((v1 (repology--string-to-version s1))
        (v2 (repology--string-to-version s2)))
    (catch :less?
      (while (or v1 v2)
        (pcase-let ((`(,r1 . ,v1)
                     (or (pop v1) repology-version-zero-component))
                    (`(,r2 . ,v2)
                     (or (pop v2) repology-version-zero-component)))
          (cond
           ;; First compare ranks, then values.
           ((/= r1 r2) (throw :less? (< r1 r2)))
           ;; Components are equal.  Try next component.
           ((equal v1 v2) nil)
           ;; Numeric components are compared... numerically.
           ((= r1 3) (throw :less? (< v1 v2)))
           ;; Alphabetic components are compared by case insensitively
           ;; comparing their first letters.
           (t (throw :less?
                     (string-lessp (substring v1 0 1) (substring v2 0 1)))))))
      ;; Strings S1 and S2 represent equal versions.
      nil)))


;;; Search functions
(defvar repology--cache (make-hash-table :test #'equal)
  "Hash table used to cache requests to Repology API.
Keys are triplets of arguments for `repology--get'.  Values are
cons cells like (TIME . REQUEST-RESULT).")

(defun repology--cache-key (action value start)
  "Return a cache key for current query.
See `repology--get' for precision about ACTION, VALUE, and START."
  (list action
        (if (not (eq action 'projects)) value
          ;; VALUE is a p-list.  Sort it in a fixed order so p-lists
          ;; sorted differently are cached the same way.  Also ignore
          ;; unknown filters.
          (let ((normalized nil))
            (dolist (prop repology--project-filters)
              (when (plist-member value prop)
                (setq normalized
                      (plist-put normalized prop (plist-get value prop)))))
            normalized))
        start))

(defun repology--cache-get (key)
  "Return cached value associated to KEY, or nil.
If the cached value is too old according to `repology-cache-duration',
reset the cache and return nil."
  (pcase (gethash key repology--cache)
    (`(,time . ,value)
     ;; Check if cached value is still valid.
     (if (> repology-cache-duration (time-to-seconds (time-since time)))
         value
       ;; Time is over: reset cache and return nil.
       (remhash key repology--cache)))
    (_ nil)))

(defun repology--cache-put (key value)
  "Cache KEY with VALUE."
  (puthash key (cons (current-time) value) repology--cache))

(defun repology--parse-json (json-string)
  "Parse a JSON string and returns an object.
JSON objects become alists and JSON arrays become lists."
  (if (null json-string)
      nil
    (let ((json-object-type 'alist)
          (json-array-type 'list))
      (condition-case err
          (json-read-from-string json-string)
        (json-readtable-error
         (message "%s: Could not parse string into an object.  See %S"
                  (error-message-string err)
                  json-string))))))

(defun repology--build-query-string (filters)
  "Build a filter string from a given FILTERS plist."
  (let ((query nil))
    (dolist (keyword repology--project-filters)
      (let ((value (plist-get filters keyword)))
        (when value
          (let ((key (substring (symbol-name keyword) 1)))
            (push (format "%s=%s"
                          (url-hexify-string key)
                          (url-hexify-string value))
                  query)))))
    (if (null query) ""
      (concat "?" (mapconcat #'identity query "&")))))

(defun repology--build-url (action value start)
  "Build a URL from an ACTION symbol.
Value is a plist if ACTION is `projects', or a string otherwise."
  (concat repology-base-url
          (symbol-name action)
          "/"
          (pcase action
            ('project value)
            ('repository (concat value "/problems"))
            ('projects
             (concat (and start (concat start "/"))
                     (repology--build-query-string value)))
            (_ (error "Unknown action: %S" action)))))

(defun repology-request (url &optional extra-headers)
  "Perform a raw HTTP request on URL.
EXTRA-HEADERS is an assoc list of headers/contents to send with
the request."
  (let* ((url-request-method "GET")
         (url-request-extra-headers extra-headers)
         (process-buffer (url-retrieve-synchronously url t)))
    (unwind-protect
        (with-current-buffer process-buffer
          (goto-char (point-min))
          (let* ((status-line-regexp
                  (rx bol
                      (one-or-more (not (any " "))) " "
                      (group (in "1-5") (= 2 digit)) " "
                      (group (one-or-more (in "A-Z" "a-z" " ")))
                      eol))
                 (status
                  (and (looking-at status-line-regexp)
                       (list :code (string-to-number (match-string 1))
                             :reason (match-string 2))))
                 (header nil)
                 (body nil))
            (forward-line)
            (while (looking-at "^\\([^:]+\\): \\(.*\\)")
              (push (match-string 1) header)
              (push (match-string 2) header)
              (forward-line))
            (forward-line)
            (unless (eobp)
              (setq body (buffer-substring (point) (point-max))))
            (append status (list :header (nreverse header) :body body))))
      (kill-buffer process-buffer))))

(defun repology--get (action value start)
  "Perform an HTTP GET request to Repology API.

ACTION is a symbol.  If it is `projects', VALUE is a plist and START a string.
Otherwise, VALUE is a string, and START is nil.

Information is returned as parsed JSON."
  (let ((key (repology--cache-key action value start)))
    (or (repology--cache-get key)
        (let ((request
                (repology-request
                 (repology--build-url action value start)
                 '(("Content-Type" . "application/json")))))
          (pcase (plist-get request :reason)
            ("OK"
             (let ((body (repology--parse-json (plist-get request :body))))
               (repology--cache-put key body)
               ;; Information from `projects' is a list of projects,
               ;; so, we can also cache each of them for a future
               ;; project lookup.
               (when (eq action 'projects)
                 (dolist (project body)
                   (let ((key (repology--cache-key
                               'project (repology-project-name project) nil))
                         (packages (repology-project-packages project)))
                     (repology--cache-put key packages))))
               ;; Return information.
               body))
            (status
             (error "Cannot retrieve information: %S" status)))))))

(defun repology-lookup-project (name)
  "List packages for project NAME.
NAME is a string.  Return a list of packages."
  (with-temp-message
      (format-message "Repology: Requesting information about `%s'..." name)
    (repology--get 'project name nil)))

(defun repology-search-projects (&rest filters)
  "Retrieve results of an advanced search in Repology.

FILTERS helps refining the search with the following keywords:

  `search'
     project name substring to look for

  `maintainer'
     return projects maintainer by specified person, as a string

  `category'
     return projects with specified category, as a string

  `inrepo'
     return projects present in specified repository, as a string

  `notinrepo'
     return projects absent in specified repository, as a string

  `repos'
     return projects present in specified number of
     repositories (exact values and open/closed ranges strings
     are allowed, e.g. \"1\", \"5-\", \"-5\", \"2-7\")

  `families'
     return projects present in specified number of repository
     families (for instance, use 1 to get unique projects)

  `repos_newest'
     return projects which are up to date in specified number of
     repositories

  `families_newest'
     return projects which are up to date in specified number of
     repository families

  `newest'
     return newest projects only

  `outdated'
     return outdated projects only

  `problematic'
     return problematic projects only

  `vulnerable'
     return projects potentially vulnerable

  `has_related'
     return projects which have related ones (may require merging)

Return a list of projects.  Projects with a known non-free license are removed
from output, unless `repology-free-only-projects' is nil."
  (let ((result nil)
        (name nil))
    (with-temp-message "Repology: Querying API..."
      (catch :exit
        (while t
          (let ((request (repology--get 'projects filters name)))
            (setq result (append result request))
            (cond
             ;; Too many matches: drop those above limit and exit.
             ((<= repology-projects-limit (length result))
              (setq result (seq-subseq result 0 repology-projects-limit))
              (throw :exit nil))
             ;; Matches exhausted: exit and return result.
             ((> repology-projects-hard-limit (length request))
              (throw :exit result))
             ;; Resume search starting from an imaginary project
             ;; located right after the last project found,
             ;; alphabetically. This is done by appending an hyphen to
             ;; the name of the last project found.
             (t
              (setq name
                    (pcase (last request)
                      (`(,(and (pred repology-project-p) project))
                       (concat (repology-project-name project) "-"))
                      (other (error "Invalid request result: %S" other))))))))))
    ;; Possibly keep only free projects.
    (if repology-free-only-projects
        (with-temp-message "Repology: Filtering out non-free projects..."
          (seq-filter #'repology-free-p result))
      result)))

(defun repology-report-problems (repository)
  "List problems related to REPOSITORY.
REPOSITORY is a string.  Return a list of problems."
  (unless (member repository (repology-list-repositories))
    (user-error "Unknown repository: %S" repository))
  (with-temp-message
      (message "Repology: Fetching problems reports about %s"
               (repology-repository-full-name repository))
    (repology--get 'repository repository nil)))


;;; Display functions
(defvar repology--display-projects-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") 'repology--show-current-project)
    (define-key map (kbd "F") 'repology--check-freedom)
    map)
  "Local keymap for `repology--display-projects-mode' buffers.")

(defvar repology--display-packages-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") 'repology--show-current-package)
    (define-key map (kbd "F") 'repology--check-freedom)
    map)
  "Local keymap for `repology--display-packages-mode' buffers.")

(defun repology--show-current-package ()
  "Display packages associated to package at point."
  (interactive)
  (repology-display-package (tabulated-list-get-id)))

(defun repology--check-freedom ()
  "Check if package or project at point is free."
  (interactive)
  (message "Freedom status: %s"
           (pcase (repology-free-p (tabulated-list-get-id))
             ('unknown (propertize "Unknown" 'face 'shadow))
             ('nil (propertize "Non-Free" 'face 'warning))
             (_ (propertize "Free" 'face 'highlight)))))

(defun repology--show-current-project ()
  "Display packages associated to project at point."
  (interactive)
  (repology-display-packages
   (repology-project-packages (tabulated-list-get-id))))

(define-derived-mode repology--display-package-mode tabulated-list-mode
  "Repology/Package"
  "Major mode used to display packages returned by Repology API.
\\{tabulated-list-mode-map}"
  (setq tabulated-list-format [("Field" 15 t) ("Value" 0 t)])
  (tabulated-list-init-header))

(define-derived-mode repology--display-packages-mode tabulated-list-mode
  "Repology/Packages"
  "Major mode used to display packages returned by Repology API.
\\{repology--display-packages-mode-map}"
  (setq tabulated-list-format
        (repology--columns-to-header repology-display-packages-columns))
  (tabulated-list-init-header))

(define-derived-mode repology--display-projects-mode tabulated-list-mode
  "Repology/Projects"
  "Major mode used to display projects returned by Repology API.
\\{repology--display-projects-mode-map}"
  (setq tabulated-list-format
        (repology--columns-to-header repology-display-projects-columns))
  (tabulated-list-init-header))

(define-derived-mode repology--display-problems-mode tabulated-list-mode
  "Repology/Problems"
  "Major mode used to display problems returned by Repology API.
\\{tabulated-list-mode-map}"
  (setq tabulated-list-format
        (repology--columns-to-header repology-display-problems-columns))
  (tabulated-list-init-header))

(defun repology--value-to-string (value)
  "Change VALUE object into a string suitable for display."
  (pcase value
    (`nil "-")
    ((pred listp)
     (mapconcat (lambda (e) (format "%s" e))
                (seq-uniq value)
                " "))
    (_
     (format "%s" value))))

(defun repology--package-status-face (package)
  "Return face associated to status from PACKAGE."
  (let ((status (repology-package-field package 'status)))
    (alist-get status repology-status-faces 'default nil #'equal)))

(defun repology--make-display (data buffer-name mode format-descriptors)
  "Display DATA in a buffer named after BUFFER-NAME string.
DATA is displayed in a major mode derived from `tabulated-list-mode', and set
by function MODE.  Each entry is identified by the element from DATA, and
formatted according to FORMAT-DESCRIPTORS function.  This function is called
with one argument: an element from DATA."
  (let ((buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (funcall mode)
      (setq tabulated-list-entries
            (mapcar (lambda (datum)
                      (list datum
                            (apply #'vector
                                   (funcall format-descriptors datum))))
                    data))
      (tabulated-list-print))
    (pop-to-buffer buffer)))

(defun repology--columns-to-header (specs)
  "Return vector of column names according to SPECS.
SPECS is an association list.  Each entry has the form (NAME _ WIDTH SORT)
where NAME, WIDTH and SORT are of the expected type in `tabulated-list-format'."
  (apply #'vector
         (mapcar (lambda (format)
                   (pcase format
                     (`(,name ,_ ,width ,sort) (list name width sort))
                     (other
                      (user-error "Invalid package column format: %S" other))))
                 specs)))

(defun repology--column-to-descriptor (datum specs &optional symbol-handler)
  "Return list of descriptors for DATUM according to SPECS.

DATUM is a package, a problem, or a project.  SPECS is an association
list.  Each entry has the form (_ VALUE _ _).

VALUE is a function called with DATUM as its sole argument.  When VALUE is
a symbol, and optional argument SYMBOL-HANDLER is a function, SYMBOL-HANDLER
is called with two arguments: DATUM and VALUE.  In any case, the return value
is then turned into a string and displayed."
  (mapcar (lambda (spec)
            (pcase spec
              ;; Contents as a function.
              (`(,_ ,(and (pred functionp) f) ,_ ,_)
               (repology--value-to-string (funcall f datum)))
              ;; Contents as a symbol.
              ((and (guard symbol-handler)
                    `(,_ ,(and (pred symbolp) field) ,_ ,_))
               (repology--value-to-string (funcall symbol-handler datum field)))
              ;; Invalid contents.
              (other (user-error "Invalid format type: %S" other))))
          specs))

(defun repology--format-field-descriptors (field)
  "Format an entry from FIELD.
Format follows `repology-display-packages-columns' specifications.
Return a list of descriptors."
  (pcase field
    (`(,name . ,value)
     (list (symbol-name name)
           (repology--value-to-string value) ))
    (_
     (error "Invalid field: %S" field))))

(defun repology--format-package-descriptors (package)
  "Format an entry from PACKAGE.
Format follows `repology-display-packages-columns' specifications.
Return a list of descriptors."
  (repology--column-to-descriptor package
                                  repology-display-packages-columns
                                  #'repology-package-field))

(defun repology--format-project-descriptors (project)
  "Format an entry for PROJECT.
Format follows `repology-display-packages-columns' specifications.
Return a list of descriptors."
  (repology--column-to-descriptor project repology-display-projects-columns))

(defun repology--format-problem-descriptors (problem)
  "Format an entry from PROBLEM.
Format follows `repology-display-problems-columns' specifications.
Return a list of descriptors."
  (repology--column-to-descriptor problem
                                  repology-display-problems-columns
                                  #'repology-problem-field))

(defun repology-display-projects-default (_ selected)
  "Return columns format rules appropriate for projects display.
SELECTED is a selected repository, i.e., the value of `:inrepo' filter,
or nil.  This is the default value for `repology-display-projects-columns'."
  `(("Project" repology-project-name 25 t)
    ;; If a repository is selected, for each project, display the
    ;; current version of the package in that repository.
    ,@(and selected
           `(("Selected"
              (lambda (project)
                (let ((current
                       (seq-find (lambda (p)
                                   (equal ,selected
                                          (repology-package-field p 'repo)))
                                 (repology-project-packages project))))
                  (repology-package-colorized-version current)))
              20
              nil)))
    ("#"
     (lambda (p) (length (repology-project-packages p)))
     5
     ,(repology-display-sort-column "#" #'repology-compare-numbers))
    ("Newest"
     repology-project-newest-version
     12
     ,(repology-display-sort-column "Newest" #'repology-compare-versions))
    ("Outdated" repology-project-outdated-versions 30 nil)))

(defun repology-display-package (package)
  "Display PACKAGE as a tabulated list."
  (repology--make-display package
                          (format "*Repology Package: %s*"
                                  (repology-package-field package 'visiblename))
                          #'repology--display-package-mode
                          #'repology--format-field-descriptors))

(defun repology-display-packages (packages)
  "Display PACKAGES as a tabulated list.
PACKAGES is a list of packages, as returned by `repology-lookup-project'.
Columns are displayed according to `repology-display-packages-columns'."
  (repology--make-display packages
                          "*Repology Packages*"
                          #'repology--display-packages-mode
                          #'repology--format-package-descriptors))

(defun repology-display-projects (projects &optional selected)
  "Display PROJECTS as a tabulated list.

PROJECTS is a list of projects, as returned by `repology-search-projects'.
Optional argument SELECTED, when non-nil, is the name of a repository to which
all projects are related.

Columns are displayed according to `repology-display-projects-columns'."
  (let ((repology-display-projects-columns
         (if (functionp repology-display-projects-columns)
             (funcall repology-display-projects-columns projects selected)
           repology-display-projects-columns)))
    (repology--make-display projects
                            "*Repology Projects*"
                            #'repology--display-projects-mode
                            #'repology--format-project-descriptors)))

(defun repology-display-problems (problems)
  "Display PROBLEMS as a tabulated list.
PROBLEMS is a list of problems, as returned by `repology-report-problems'.
Columns are displayed according to `repology-display-problems-columns'."
  (repology--make-display problems
                          "*Repology Problems*"
                          #'repology--display-problems-mode
                          #'repology--format-problem-descriptors))


;;; Interactive query
(defconst repology--main-prompt
  (format-message
   "Action: [S]earch projects  [L]ookup project  \
\[R]eport repository problems    (`q' to quit)")
  "Main prompt used if `repology' UI.")

(defun repology--select-key (allowed-keys msg)
  "Keep requesting user to press a key until it belongs to ALLOWED-KEYS.
ALLOWED-KEYS is a list of characters.  MSG is the message used as the prompt."
  (let ((key (read-char msg)))
    (while (not (memq key allowed-keys))
      (message "Invalid key")
      (sit-for 0.5)
      (setq key (read-char msg)))
    key))

(defun repology--query-y-or-n-p (prompt _)
  "Ask user a \"y or n\" question, displaying PROMPT.
Return \"on\" or \"off\"."
  (if (y-or-n-p prompt) "on" "off"))

(defun repology--query-repository (prompt initial)
  "Ask user an existing repository by its full name, displaying PROMPT.
INITIAL is the initial input.  Return a repository internal name."
  (repology-repository-name
   (completing-read prompt (repology-list-repositories t) nil t initial)))

(defun repology--query-filter-value (filter initial)
  "Ask user for FILTER value.
FILTER is a project filter, as a keyword.  INITIAL is a string inserted as
a first suggestion, or nil.  Return the answer as a string."
  (pcase (assq filter repology-project-filters-parameters)
    (`nil
     (error "Unknown filter: %S" filter))
    (`(,_ ,prompt nil)
     (read-string prompt initial))
    (`(,_ ,prompt ,(and (pred functionp) collection))
     (funcall collection prompt initial))
    (other
     (error "Invalid value: %S" other))))

;;;###autoload
(defun repology ()
  "Query Repology interactively.

This function interacts with Repology API in three ways.  You can:

1. List all packages associated to a given project.  See function
   `repology-lookup-project'.

2. Find potential problems related to packages in a repository, using
   `repology-report-problems'.  The function provides the list of
   repositories to choose from.

3. Search for projects matching some criteria.  Here, you build incrementally
   a filter by selecting properties from a list.  See `repology-search-projects'
   for more information.  Select \"OK\" to actually send the request.

   During the filter creation, you may change the maximum number of projects
   displayed by selecting \"limit\" from the list of properties.  The default
   value is `repology-projects-limit'."
  (interactive)
  (pcase (repology--select-key '(?s ?S ?l ?L ?r ?R ?q ?Q) repology--main-prompt)
    ((or ?r ?R)
     (repology-display-problems
      (repology-report-problems
       (repology--query-repository "Repository: " nil))))
    ((or ?l ?L)
     (repology-display-packages
      (repology-lookup-project (read-string "Project: "))))
    ((or ?s ?S)
     (let* ((query nil)
            (limit repology-projects-limit)
            (answers
             ;; Trim colons from completion for easier readability.
             ;; Add the special "limit" and "OK" values.  Emphasize
             ;; the latter.
             (append (mapcar (lambda (k) (substring (symbol-name k) 1))
                             repology--project-filters)
                     `("limit" ,(propertize "OK" 'face 'warning))))
            (query-filter
             (lambda (p)
               ;; Ask user for a filter.  P is the property list
               ;; built so far.  Return associated keyword.
               (let ((prompt (format "Filter %s [limit:%d]: "
                                     (if p (format "%S" p) "()")
                                     limit)))
                 (read
                  (concat ":" (completing-read prompt answers nil t)))))))
       ;; Build filters incrementally.
       (catch :exit
         (while t
           (let ((filter (funcall query-filter query)))
             (pcase filter
               (:OK
                (throw :exit nil))
               (:limit
                (setq limit (read-number "Temporary limit: " limit)))
               (_
                (let* ((last (plist-get query filter))
                       (value (repology--query-filter-value filter last)))
                  (setq query (plist-put query filter value))))))))
       ;; Eventually send complete request to Repology API.
       (repology-display-projects (let ((repology-projects-limit limit))
                                    (apply #'repology-search-projects query))
                                  ;; Selected repository, or nil.
                                  (plist-get query :inrepo))))
    ((or ?q ?Q)
     (message "Repology: Quitting"))
    (_
     (error "This should not happen"))))


(provide 'repology)
;;; repology.el ends here
