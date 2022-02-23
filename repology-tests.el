;;; repology-tests.el --- Test for Repology library  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nicolas Goaziou

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

;;; Code:

(require 'repology)

(ert-deftest repology-version-< ()
  "Test `repology-version-<'."
  (should (repology-version-< "1.0alpha1" "1.0beta1"))
  (should (repology-version-< "1.0beta1" "1.0"))
  (should (repology-version-< "1.0" "1.0patch1"))
  (should (repology-version-< "1.0patch1" "1.0.1"))
  (should (repology-version-< "1.0.1" "1.0a"))
  (should (repology-version-< "1.0a" "1.0b"))
  (should (repology-version-< "1.0b" "1.1"))
  (should (repology-version-< "1.1" "1.2")))

(provide 'repology-tests)
;;; repology-tests.el ends here
