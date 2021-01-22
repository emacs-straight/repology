;;; repology-utils.el --- Utilitaries for Repology   -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <mail@nicolasgoaziou.fr>

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

;; This library provides various user-facing tools.  They include accessors and
;; predicates for package, project and problem objects, in addition to more
;; specialized functions useful in configuration variables.

;; The library also provides the low-level `repology-request', used to
;; perform a raw HTTP request on a given URL.

;;; Code:


;;; Packages
(defun repology-package-p (object)
  "Return t if OBJECT is a package."
  (and (consp object)
       ;; Mandatory fields.
       (stringp (alist-get 'repo object))
       (stringp (or (alist-get 'name object)
                    (alist-get 'srcname object)
                    (alist-get 'binname object)))
       (stringp (alist-get 'version object))))

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

(defun repology--package-status-face (package)
  "Return face associated to status from PACKAGE."
  (let ((status (repology-package-field package 'status)))
    (if (member status repology-package-all-status)
        (intern (format "repology-%s-status" status))
      ;; If package status list is not up-to-date, fall back to
      ;; `default' face.
      (warn "Repology: Unknown package status: %S; Using `default' face" other)
      'default)))

(defun repology-package-colorized-status (package)
  "Return colorized status string for PACKAGE.
The version string is emphasized according to PACKAGE's status.
Return nil if PACKAGE has no status field."
  (let ((status (repology-package-field package 'status)))
    (and (stringp status)
         (propertize status 'face (repology--package-status-face package)))))

(defun repology-package-colorized-version (package)
  "Return colorized version string for PACKAGE.
The version string is emphasized according to PACKAGE's status."
  (propertize (repology-package-field package 'version)
              'face
              (repology--package-status-face package)))


;;; Projects
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
          (lambda (s1 s2) (repology-version-< s2 s1)))))


;;; Problems
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


;;; Repositories
(defvar repology--repositories nil
  "List of repositories known to Repology.
The list is populated by `repology-list-repositories'.  Call that function
instead of using this variable.")

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
                 (start 0)
                 (internal-re (rx "id=\"" (group (+? nonl)) "\""))
                 (full-re (rx "href=\"/repository/"
                              (+? anychar)
                              "\">"
                              (group (+? anychar))
                              "<")))
             (while (string-match internal-re body start)
               (setq start (match-end 0))
               (let* ((repo (match-string 1 body))
                      (true-name
                       (and (string-match full-re body start)
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


;;; Requests
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
            (while (looking-at (rx line-start (group (+? nonl)) ": "))
              (push (match-string 1) header)
              (push (buffer-substring (match-end 0) (line-end-position)) header)
              (forward-line))
            (forward-line)
            (unless (eobp)
              (setq body (buffer-substring (point) (point-max))))
            (append status (list :header (nreverse header) :body body))))
      (kill-buffer process-buffer))))


;;; Version Comparison

;; This part implements version comparison as done by Repology's
;; libversion.  See
;; <https://github.com/repology/libversion/blob/master/doc/ALGORITHM.md>.
(defconst repology-version-zero-component '(1 . 0)
  "Version component representing 0 or any missing component.")

(defconst repology-version-pre-keywords '("alpha" "beta" "rc" "pre")
  "List of pre-release keywords in version strings.")

(defconst repology-version-post-keywords '("patch" "post" "pl" "errata")
  "List of post-release keywords in version strings.")

(defun repology--string-to-version (s)
  "Return version associated to string S.
Version is a list of components (RANK . VALUE) suitable for comparison, with
the function `repology-version-<'."
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

(defun repology-version-< (v1 v2)
  "Return t if version V1 is lower (older) than version V2.
V1 and V2 are strings."
  (let ((v1 (repology--string-to-version v1))
        (v2 (repology--string-to-version v2)))
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
      ;; Strings V1 and V2 represent equal versions.
      nil)))


;;; Sort Functions
(defun repology--display-sort-generic (e1 e2 predicate)
  "Compare entries E1 and E2 according to PREDICATE.
E1 and E2 are elements of `tabulated-list-entries'.  PREDICATE is called on the
values from E1 and E2 at the column being sorted."
  (let ((column
         ;; Find number of column being sorted.
         (seq-position tabulated-list-format
                       (car tabulated-list-sort-key)
                       (pcase-lambda (`(,n . ,_) s) (equal n s)))))
    ;; Call PREDICATE on values instead of entries.
    (let ((val1 (elt (cadr e1) column))
          (val2 (elt (cadr e2) column)))
      (funcall predicate val1 val2))))

(defun repology-display-sort-numbers (e1 e2)
  "Return t if E1 is numerically less than E2.
E1 and E2 are elements of `tabulated-list-entries'."
  (repology--display-sort-generic
   e1 e2
   (lambda (s1 s2) (< (string-to-number s1) (string-to-number s2)))))

(defun repology-display-sort-texts (e1 e2)
  "Return t if E1 is before E2, in collation order.
E1 and E2 are elements of `tabulated-list-entries'.  Case is ignored."
  (repology--display-sort-generic
   e1 e2
   (lambda (s1 s2) (string-collate-lessp s1 s2 nil t))))

(defun repology-display-sort-versions (e1 e2)
  "Return t if E1 is older than E2.
E1 and E2 are elements of `tabulated-list-entries'."
  (repology--display-sort-generic e1 e2 #'repology-version-<))


(provide 'repology-utils)
;;; repology-utils.el ends here
