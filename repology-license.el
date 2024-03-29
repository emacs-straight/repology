;;; repology-license.el --- Freedom check for Repology  -*- lexical-binding: t; -*-

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

;; This library provides the `repology-check-freedom' function, which returns
;; t when a package or a project can be considered as free, nil it is
;; identified as being non-free, and `unknown' otherwise.

;; The decision is made by polling a number of "Reference repositories",
;; defined in `repology-license-reference-repositories'.  If the ratio of
;; "Free" votes is above `repology-license-poll-threshold', the project is
;; declared as free.

;; In order to see the results of each vote, and possibly debug the process,
;; you can set `repology-license-debug' to a non-nil value.

;;; Code:
(require 'repology-utils)


;;; Constants
(defconst repology-license-reference-repositories
  '(("debian"    "main"     t)
    ("debian"    "contrib"  t)
    ("^mageia"   "core"     t)
    ("gnuguix"   nil        t)
    ("hyperbola" nil        t)
    ("parabola"  nil        t)
    ("pureos"    nil        t)
    ("trisquel"  nil        t)
    ("gnu_elpa"  nil        t)
    ("^fedora"   nil        repology--license-check:fedora)
    ("^gentoo$"  nil        repology--license-check:gentoo)
    ("^opensuse" "/oss"     repology--license-check:opensuse-oss)
    ("debian"    "non-free" nil)
    ("^mageia"   "nonfree"  nil)
    ("^opensuse" "/non-oss" nil))
  "List of repositories providing a reliable license information.

This is a list of triplets (REPO SUBREPO PREDICATE) where:

 REPO is a regexp matching the internal name of a repository;
 SUBREPO is a regexp matching a sub-repository or nil;
 PREDICATE is either a boolean or a function called with one string argument.

When PREDICATE is a function, a return value of t means the argument is a free
license according to the repository, whereas nil means it is non-free.  Any
other value means the repository cannot decide, and pass.  In this case, the
vote does not count.

If PREDICATE is t, we trust the repository to provide only free software.
Conversely, PREDICATE is nil when the repository is known to reference only
non-free software.

A repository with a PREDICATE function is expected to have the following
properties:
- it audits carefully the licenses it reports;
- it uses a consistent, and documented, license syntax;
- Repology properly fetches the licenses of its packages.
  See URL `https://repology.org/repositories/fields'.")

(defconst repology-license-poll-threshold 0.5
  "Ratio of votes above which a package is declared to be free.")

(defconst repology--license-identifiers-url:gentoo
  "https://gitweb.gentoo.org/repo/gentoo.git/plain/profiles/license_groups"
  "URL referencing Gentoo free license identifiers.")

(defconst repology--license-categories:gentoo
  '("GPL-COMPATIBLE" "FSF-APPROVED" "OSI-APPROVED" "MISC-FREE"
    "FSF-APPROVED-OTHER" "MISC-FREE-DOCS")
  "List of free license categories according to Gentoo.")


;;; Tools
(defun repology--license-interpret-vote (free votes)
  "Return freedom vote result as nil, t or `unknown'.
FREE is the number of \"Free\" votes.  VOTES is the total number of votes."
  (cond ((= votes 0) 'unknown)
        ((> (/ (float free) votes) repology-license-poll-threshold) t)
        (t nil)))


;;; Reference Repository: Fedora
(defun repology--license-check:fedora (license)
  "Return a non-nil value if LICENSE is free, according to Fedora.
See URL \
`https://docs.fedoraproject.org/en-US/packaging-guidelines/LicensingGuidelines/'"
  (let ((case-fold-search t)
        ;; Anything in Fedora is considered to be free, unless its
        ;; license contains the following.
        (non-free-license-re
         (rx word-start "Redistributable, no modification permitted" word-end)))
    (not (string-match non-free-license-re license))))


;;; Reference Repository: Gentoo
(defvar repology--license-identifiers:gentoo nil
  "List of identifiers considered as free licenses by Gentoo.
See URL `https://wiki.gentoo.org/wiki/License_groups'.
This list is populated with `repology--license-get-identifiers:gentoo'.")

(defun repology--license-get-identifiers:gentoo ()
  "Return list of free license identifiers according to Gentoo."
  (or repology--license-identifiers:gentoo
      (with-temp-message "Repology: Fetching license identifiers for Gentoo..."
        (let ((request
               (repology-request repology--license-identifiers-url:gentoo)))
          (pcase (plist-get request :reason)
            ("OK"
             (let ((identifiers nil))
               (with-temp-buffer
                 (insert (plist-get request :body))
                 (dolist (category repology--license-categories:gentoo)
                   (goto-char 1)
                   (let ((regexp (rx line-start (literal category) (+ space))))
                     (when (re-search-forward regexp)
                       (let ((l (buffer-substring (point) (line-end-position))))
                         (setq identifiers
                               (nconc (split-string l) identifiers))))))
                 (dolist (category repology--license-categories:gentoo)
                   (setq identifiers
                         (delete (concat "@" category) identifiers))))
               (setq repology--license-identifiers:gentoo identifiers)))
            (_
             (warn "Repology: Cannot fetch Gentoo licenses.  \
Ignoring repository")
             nil))))))

(defun repology--license-gentoo:skip-whitespace ()
  "Skip past the whitespace at point."
  (skip-chars-forward " \t"))

(defun repology--license-gentoo:skip-non-whitespace ()
  "Skip past the whitespace at point."
  (skip-chars-forward "^ \t"))

(defun repology--license-gentoo:advance (&optional n)
  "Advance N characters forward."
  (forward-char n))

(defun repology--license-gentoo:peek ()
  "Return the character at point."
  (following-char))

(defun repology--license-gentoo:and ()
  "Return license freedom for the \"and\" construct at point."
  ;; Skip past "(" token.
  (repology--license-gentoo:advance 1)
  (repology--license-gentoo:skip-whitespace)
  (let ((value t))
    (while (/= (repology--license-gentoo:peek) ?\))
      (repology--license-gentoo:skip-whitespace)
      (unless (repology--license-gentoo:read-next)
        (setq value nil)))
    ;; Skip past ")" character, and to next token.
    (repology--license-gentoo:advance 1)
    (repology--license-gentoo:skip-whitespace)
    value))

(defun repology--license-gentoo:or ()
  "Return license freedom for the \"or\" construct at point."
  ;; Skip past "|| (" token.
  (repology--license-gentoo:advance 4)
  (repology--license-gentoo:skip-whitespace)
  (let ((value nil))
    (while (/= (repology--license-gentoo:peek) ?\))
      (when (repology--license-gentoo:read-next)
        (setq value t)))
    ;; Skip past ")" character, and to next token.
    (repology--license-gentoo:advance 1)
    (repology--license-gentoo:skip-whitespace)
    value))

(defun repology--license-gentoo:identifier ()
  "Return freedom of the license identifier at point."
  (let ((origin (point)))
    (repology--license-gentoo:skip-non-whitespace)
    (let ((value (member-ignore-case (buffer-substring origin (point))
                                     repology--license-identifiers:gentoo)))
      (repology--license-gentoo:skip-whitespace)
      value)))

(defun repology--license-gentoo:parameter ()
  "Assume parameter at point is set, and return license freedom accordingly."
  ;; Skip past parameter.
  (repology--license-gentoo:skip-non-whitespace)
  (repology--license-gentoo:skip-whitespace)
  (repology--license-gentoo:and))

(defun repology--license-gentoo:read-next ()
  "Return license freedom for next token, and move point past it."
  (let ((parameter-re (rx (opt "!") (one-or-more (any alnum "-" "_")) "?")))
    (pcase (repology--license-gentoo:peek)
      (?\| (repology--license-gentoo:or))
      (?\( (repology--license-gentoo:and))
      ((guard (looking-at parameter-re))
       (repology--license-gentoo:parameter))
      (_
       (repology--license-gentoo:identifier)))))

(defun repology--license-check:gentoo (license)
  "Return a non-nil value if LICENSE is free, according to Gentoo."
  (if (null (repology--license-get-identifiers:gentoo))
      'pass                             ;no license to check
    (with-temp-buffer
      (insert license)
      (goto-char 1)
      (repology--license-gentoo:skip-whitespace)
      (let ((value (not (eobp))))       ;blank string check
        (while (and value (/= (repology--license-gentoo:peek) 0))
          (unless (repology--license-gentoo:read-next)
            (setq value nil)))
        ;; Return a boolean.
        (and value t)))))


;;; Reference Repository: OpenSUSE (OSS)
(defun repology--license-check:opensuse-oss (license)
  "Return a non-nil value if LICENSE is free, according to OpenSUSE (OSS).
See URL `https://en.opensuse.org/openSUSE:Packaging_guidelines#Licensing'."
  (let ((case-fold-search t)
        ;; Anything in OSS sub-repository from OpenSUSE is considered
        ;; to be free, unless its license contains the following.
        (non-free-license-re
         (rx word-start "SUSE-Firmware" word-end)))
    (not (string-match non-free-license-re license))))


;;; License Check Debugging
(defvar repology-license-debug nil
  "When non-nil, display explanations when a project declared non-free.
Information is displayed in \"*Repology: License Debug*\" buffer.")

(defun repology--license-debug-line (package freedom)
  "Format license debug information for PACKAGE.
When FREEDOM is t, declare PACKAGE was reported as free.  If it is nil,
declare it as non-free.  Otherwise report an unknown status."
  (let ((repo (repology-package-field package 'repo))
        (subrepo (repology-package-field package 'subrepo))
        (name (repology-package-field package 'visiblename)))
    (format "%s (%s%s) => %s\n"
            name
            repo
            (if subrepo (concat "/" subrepo) "")
            (pcase freedom
              ('unknown "Unknown")
              ('nil "Non-Free")
              (_ "Free")))))

(defun repology--license-debug-display (project reports free votes)
  "Print license check output for non-free PROJECT.
REPORTS is a list of strings, as returned by `repology--license-debug-line'.
FREE is the number of free packages in PROJECT.  VOTES is the number of packages
from reference repositories in PROJECT."
  (with-current-buffer (get-buffer-create "*Repology: License Debug*")
    (insert (format "=== Project %S: %s (ratio: %.2f) ===\n"
                    (repology-project-name project)
                    (pcase (repology--license-interpret-vote free votes)
                      ('unknown "UNKNOWN")
                      ('nil "NON-FREE")
                      (_ "FREE"))
                    (if (= votes 0)
                        0
                      (/ (float free) votes))))
    (apply #'insert reports)
    (insert "\n")))


;;; Main Function
(defun repology--license-find-reference-repository (package)
  "Return the reference repository containing PACKAGE, or nil.
Return value is a triplet from `repology-license-reference-repositories'."
  (let ((repo (repology-package-field package 'repo))
        (subrepo (repology-package-field package 'subrepo)))
    (seq-find (pcase-lambda (`(,r ,s ,_))
                (and (string-match r repo)
                     (or (not s)
                         (and subrepo (string-match s subrepo)))))
              repology-license-reference-repositories)))

(defun repology--license-vote (repository package)
  "Return REPOSITORY vote concerning PACKAGE freedom.
REPOSITORY is an element from `repology-license-reference-repositories'.
PACKAGE is a package object."
  (pcase (or repository (repology--license-find-reference-repository package))
    (`(,_ ,_ ,(and (pred functionp) p))
     (catch :vote
       (dolist (l (repology-package-field package 'licenses))
         ;; For a vote to be "Free", every license string must be
         ;; free.  However, we stop at first "Unknown" or "Non-Free"
         ;; result.
         (pcase (funcall p l)
           ('t nil)
           (other (throw :vote other))))
       t))
    (`(,_ ,_ ,boolean) boolean)
    (other (error "Wrong repository definition: %S" other))))

(defun repology-check-freedom (object)
  "Return t when project or package OBJECT is free.

A package is free when any reference repository can attest it uses only free
licenses.  See `repology-license-reference-repositories' for a list of such
repositories.  If the package does not belong to any of these repositories,
or if there is not enough information to decide, return `unknown'.  Otherwise,
return nil.

A project is free if the ratio of free packages among the packages in project
from reference repositories is above `repology-license-poll-threshold'.
If the project does not contain any package from such repositories, or if those
repositories cannot decide, return `unknown'.  In any other case, return nil.

Of course, it is not a legal statement, merely an indication."
  (pcase object
    ((pred repology-package-p)
     (pcase (repology--license-find-reference-repository object)
       ('nil 'unknown)
       (repository
        (let ((decision (repology--license-vote repository object)))
          (if (booleanp decision) decision 'unknown)))))
    ((pred repology-project-p)
     (let ((votes 0)
           (yes 0)
           (reports nil)
           (voters nil))
       (dolist (package (repology-project-packages object))
         (pcase (repology--license-find-reference-repository package)
           ('nil nil)
           (repository
            (unless (member repository voters)
              (push repository voters)  ;a repository votes only once
              (let ((free (repology--license-vote repository package)))
                (when (booleanp free)   ;has repository an opinion?
                  (cl-incf votes)
                  (when free (cl-incf yes))
                  (when repology-license-debug
                    (push (repology--license-debug-line package free)
                          reports))))))))
       ;; Maybe display vote reports as debugging information.
       (when repology-license-debug
         (repology--license-debug-display object reports yes votes))
       ;; Return value.
       (repology--license-interpret-vote yes votes)))
    (_ (user-error "Wrong argument type: %S" object))))

(provide 'repology-license)
;;; repology-license.el ends here
