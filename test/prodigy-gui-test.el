;;; prodigy-gui-test.el --- Prodigy: GUI tests -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file contains tests related to the Prodigy GUI.

;;; Code:


;;;; prodigy-marked-col

(ert-deftest prodigy-marked-col-test/marked ()
  (let ((service '(:marked t)))
    (should (string= (prodigy-marked-col service) "*"))))

(ert-deftest prodigy-marked-col-test/not-marked ()
  (let ((service '(:marked nil)))
    (should (string= (prodigy-marked-col service) ""))))


;;;; prodigy-name-col

(ert-deftest prodigy-name-col-test ()
  (let ((service '(:name "foo")))
    (should (string= (prodigy-name-col service) "foo"))))


;;;; prodigy-status-col

(ert-deftest prodigy-status-col-test ()
  (with-mock
   (stub prodigy-status-name => "Foo")
   (stub prodigy-status-face => 'foo)
   (let ((service '(:name "foo")))
     (should (equal (prodigy-status-col service) #("Foo" 0 3 (face foo)))))))


;;;; prodigy-tags-col

(ert-deftest prodigy-tags-col-test/no-tags ()
  (let ((service '(:name "service")))
    (should (string= (prodigy-tags-col service) ""))))

(ert-deftest prodigy-tags-col-test/single-tag ()
  (let ((service '(:name "service" :tags (foo))))
    (should (string= (prodigy-tags-col service) "foo"))))

(ert-deftest prodigy-tags-col-test/multiple-tags ()
  (let ((service '(:name "service" :tags (foo bar baz))))
    (should (string= (prodigy-tags-col service) "foo, bar, baz"))))

(ert-deftest prodigy-tags-col-test/with-hidden-tags ()
  (let ((prodigy-tags
         '((:name foo :hide t)
           (:name baz :hide t)))
        (service '(:name "service" :tags (foo bar baz qux))))
    (should (string= (prodigy-tags-col service) "bar, qux"))))


;;;; prodigy-list-entries


;;;; prodigy-service-at-pos


;;;; prodigy-service-at-pos-p


;;;; prodigy-goto-next-line


;;;; prodigy-goto-prev-line


;;;; prodigy-goto-first-line


;;;; prodigy-goto-last-line


;;;; prodigy-goto-pos


;;;; prodigy-status-name


;;;; prodigy-status-face


;;;; prodigy-relevant-services


;;;; prodigy-set-default-directory


(provide 'prodigy-gui-test)

;;; prodigy-gui-test.el ends here
