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
;; `repology-free-only-projects'.  The function `repology-check-freedom'
;; is responsible for guessing if a project, or a package, is free.

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


;;; Load Libraries
(require 'json)
(require 'tabulated-list)
(require 'url)

;; These need to be loaded after upstream constants.
(require 'repology-utils)
(require 'repology-license)


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
  "When t, return only free projects from searches.

Declaring a project as free the consequence of a very conservative process.
Free projects with missing licensing information, or too confidential, may be
ignored.  You can circumvent this by setting the value to `include-unknown'.
In this case searches also include every project not clearly identified as
non-free.

A value of nil includes all projects.

See `repology-check-freedom' for more information."
  :type '(choice
          (const :tag "Only free projects" t)
          (const :tag "Free and unknown projects" include-unknown)
          (const :tag "Every project" nil)))

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
  '(("Repository"
     repology-package-repository-full-name
     20
     repology-display-sort-texts)
    ("Name" visiblename 20 t)
    ("Version"
     repology-package-colorized-version
     12
     repology-display-sort-versions)
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

You may also want to look into comparison functions suitable for SORT, such as
`repology-display-sort-numbers', `repology-display-sort-texts', and
`repology-display-sort-versions'."
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

You may also want to look into comparison functions suitable for SORT, such as
`repology-display-sort-numbers', `repology-display-sort-texts', and
`repology-display-sort-versions'."
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
    (if (not repology-free-only-projects) result
      (with-temp-message "Repology: Filtering out non-free projects..."
        (seq-filter (if (eq repology-free-only-projects 'include-unknown)
                        #'repology-check-freedom
                      (lambda (p) (eq t (repology-check-freedom p))))
                    result)))))

(defun repology-report-problems (repository)
  "List problems related to REPOSITORY.
REPOSITORY is a string.  Return a list of problems."
  (unless (member repository (repology-list-repositories))
    (user-error "Unknown repository: %S" repository))
  (with-temp-message
      (format "Repology: Fetching problems reports about %s"
              (repology-repository-full-name repository))
    (repology--get 'repository repository nil)))


;;; Display functions
(defvar repology--display-projects-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") 'repology--show-current-project)
    (define-key map (kbd "F") 'repology--check-freedom-at-point)
    map)
  "Local keymap for `repology--display-projects-mode' buffers.")

(defvar repology--display-packages-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") 'repology--show-current-package)
    (define-key map (kbd "F") 'repology--check-freedom-at-point)
    map)
  "Local keymap for `repology--display-packages-mode' buffers.")

(defun repology--show-current-package ()
  "Display packages associated to package at point."
  (interactive)
  (repology-display-package (tabulated-list-get-id)))

(defun repology--check-freedom-at-point ()
  "Check if package or project at point is free."
  (interactive)
  (message "Freedom status: %s"
           (pcase (repology-check-freedom (tabulated-list-get-id))
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
     repology-display-sort-numbers)
    ("Newest" repology-project-newest-version 12 repology-display-sort-versions)
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
  "Main prompt used in `repology' UI.")

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
