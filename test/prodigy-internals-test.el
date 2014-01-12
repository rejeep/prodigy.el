;;; prodigy-internals-test.el --- Prodigy: Tests for the internal functions -*- lexical-binding: t; -*-

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

;;; Code:

(require 'ert-async)


;;;; prodigy-with-refresh

(ert-deftest prodigy-with-refresh-test ()
  (with-mock
   (mock (prodigy-refresh) :times 1)
   (prodigy-with-refresh)))


;;;; prodigy-taggable-tags

(ert-deftest prodigy-taggable-tags-test ()
  (with-sandbox
   (let ((prodigy-tags
          '((:name foo)
            (:name baz)))
         (taggable
          '(:tags (foo bar baz))))
     (should (equal (prodigy-taggable-tags taggable) '((:name foo) (:name baz)))))))


;;;; prodigy-find-tag

(ert-deftest prodigy-find-tag-test ()
  (with-sandbox
   (let ((prodigy-tags
          '((:name foo)
            (:name baz))))
     (should (equal (prodigy-find-tag 'foo) '(:name foo)))
     (should-not (prodigy-find-tag 'bar))
     (should (equal (prodigy-find-tag 'baz) '(:name baz))))))


;;;; prodigy-resolve-pathy

(ert-deftest prodigy-resolve-pathy-test ()
  (should (equal (prodigy-resolve-pathy "foo") '("foo")))
  (should (equal (prodigy-resolve-pathy '("foo" "bar")) '("foo" "bar")))
  (should (equal (prodigy-resolve-pathy '("foo" "bar" "foo")) '("foo" "bar")))
  (should (equal (prodigy-resolve-pathy (lambda () "foo")) '("foo")))
  (should (equal (prodigy-resolve-pathy (lambda () '("foo" "bar"))) '("foo" "bar")))
  (should (equal (prodigy-resolve-pathy (lambda () '("foo" "bar" "foo"))) '("foo" "bar"))))


;;;; prodigy-every

(ert-deftest-async prodigy-every-test (done)
  (let ((count 0))
    (prodigy-every 1
        (lambda (next)
          (setq count (1+ count))
          (if (= count 3)
              (funcall done)
            (funcall next))))))


;;;; prodigy-service-stopping-p

(ert-deftest prodigy-service-stopping-p-test ()
  (should (prodigy-service-stopping-p '(:status stopping)))
  (should-not (prodigy-service-stopping-p '(:status not-stopping))))


;;;; prodigy-maybe-kill-process-buffer

(ert-deftest prodigy-maybe-kill-process-buffer-test/default-dont-kill-buffer ()
  (with-mock
   (not-called kill-buffer)
   (let ((service (prodigy-test/make-service)))
     (prodigy-maybe-kill-process-buffer service))))

(ert-deftest prodigy-maybe-kill-process-buffer-test/service-kill-process ()
  (with-mock
   (stub get-buffer => "test-service")
   (mock (kill-buffer "test-service") :times 1)
   (let ((service (prodigy-test/make-service
                   :kill-process-buffer-on-stop t)))
     (prodigy-maybe-kill-process-buffer service))))

(ert-deftest prodigy-maybe-kill-process-buffer-test/global-kill-process ()
  (with-mock
   (stub get-buffer => "test-service")
   (mock (kill-buffer "test-service") :times 1)
   (let ((prodigy-kill-process-buffer-on-stop t)
         (service (prodigy-test/make-service)))
     (prodigy-maybe-kill-process-buffer service))))


;;;; prodigy-service-started-p

(ert-deftest prodigy-service-started-p-test/no-process ()
  (let ((service (prodigy-test/make-service)))
    (should-not (prodigy-service-started-p service))))

(ert-deftest prodigy-service-started-p-test/process-not-live ()
  (with-mock
   (stub process-live-p)
   (let ((service (prodigy-test/make-service :process t)))
     (should-not (prodigy-service-started-p service)))))

(ert-deftest prodigy-service-started-p-test/process-live ()
  (with-mock
   (stub process-live-p => t)
   (let ((service (prodigy-test/make-service :process t)))
     (should (prodigy-service-started-p service)))))


;;;; prodigy-service-first-tag-with

(ert-deftest prodigy-service-first-tag-with-test/service-has-property ()
  (with-sandbox
   (let ((service (prodigy-test/make-service :foo "bar")))
     (should (equal (prodigy-service-first-tag-with service :foo) "bar")))))

(ert-deftest prodigy-service-first-tag-with-test/service-has-property-is-nil ()
  (with-sandbox
   (let ((prodigy-tags
          '((:name bar :foo "bar")))
         (service (prodigy-test/make-service :foo nil :tags '(bar))))
     (should-not (prodigy-service-first-tag-with service :foo)))))

(ert-deftest prodigy-service-first-tag-with-test/service-does-not-have-property ()
  (with-sandbox
   (let ((prodigy-tags
          '((:name bar :foo "bar")))
         (service (prodigy-test/make-service :tags '(bar))))
     (should (string= (prodigy-service-first-tag-with service :foo) "bar")))))


;;;; prodigy-services

(ert-deftest prodigy-services-test/no-filters ()
  (with-sandbox
   (prodigy-define-service :name "foo")
   (prodigy-define-service :name "bar")
   (should (equal (prodigy-services) '((:name "bar") (:name "foo"))))))

(ert-deftest prodigy-services-test/name-filter ()
  (with-sandbox
   (prodigy-define-service :name "foo")
   (prodigy-define-service :name "bar")
   (prodigy-define-service :name "baz")
   (prodigy-add-filter :name "ba")
   (should (equal (prodigy-services) '((:name "baz") (:name "bar"))))))

(ert-deftest prodigy-services-test/tags-filter ()
  (with-sandbox
   (prodigy-define-service :name "foo" :tags '(qux))
   (prodigy-define-service :name "bar")
   (prodigy-define-service :name "baz")
   (prodigy-add-filter :tag 'qux)
   (should (equal (prodigy-services) '((:name "foo" :tags (qux)))))))

(ert-deftest prodigy-services-test/name-and-tags-filter ()
  (with-sandbox
   (prodigy-define-service :name "foo")
   (prodigy-define-service :name "bar" :tags '(qux))
   (prodigy-define-service :name "baz")
   (prodigy-add-filter :tag 'qux)
   (prodigy-add-filter :name "ba")
   (should (equal (prodigy-services) '((:name "bar" :tags (qux)))))))


;;;; prodigy-find-status

(ert-deftest prodigy-find-status-test/no-such-status ()
  (should-not (prodigy-find-status 'foo)))

(ert-deftest prodigy-find-status-test/status-exists ()
  (with-sandbox
   (prodigy-define-status :id 'foo)
   (should (prodigy-find-status 'foo))))

(ert-deftest prodigy-find-status-test/id-nil ()
  (with-sandbox
   (prodigy-define-status :id 'stopped)
   (should (prodigy-find-status nil))))


;;;; prodigy-start-status-check-timer

(ert-deftest prodigy-start-status-check-timer-test/exists ()
  (with-mock
   (let ((prodigy-timer 'timer))
     (not-called prodigy-service-status-check)
     (not-called prodigy-every)
     (prodigy-start-status-check-timer))))

(ert-deftest prodigy-start-status-check-timer-test/does-not-exist ()
  (with-mock
   (mock (prodigy-service-status-check) :times 1)
   (mock (prodigy-every) :times 1)
   (prodigy-start-status-check-timer)))


;;;; prodigy-service-status-check


;;;; prodigy-tags

(ert-deftest prodigy-tags-test ()
  (with-sandbox
   (prodigy-test/make-service :name "service-1" :tags '(foo bar))
   (prodigy-test/make-service :name "service-2" :tags '(bar baz))
   (prodigy-test/make-service :name "service-3" :tags '(qux))
   (should (equal (prodigy-tags) '(qux bar baz foo)))))


;;;; prodigy-service-tagged-with?

(ert-deftest prodigy-service-tagged-with?-test ()
  (with-sandbox
   (let ((service (prodigy-test/make-service :tags '(foo))))
     (should (prodigy-service-tagged-with? service 'foo))
     (should-not (prodigy-service-tagged-with? service 'bar)))))


;;;; prodigy-services-tagged-with

(ert-deftest prodigy-services-tagged-with-test ()
  (with-sandbox
   (let ((service-1 (prodigy-test/make-service :name "service-1" :tags '(foo bar)))
         (service-2 (prodigy-test/make-service :name "service-2" :tags '(bar baz)))
         (service-3 (prodigy-test/make-service :name "service-3" :tags '(qux))))
     (should (equal (prodigy-services-tagged-with 'bar) (list service-2 service-1))))))


;;;; prodigy-marked-services

(ert-deftest prodigy-marked-services-test ()
  (with-sandbox
   (let ((service-1 (prodigy-test/make-service :name "service-1" :marked t))
         (service-2 (prodigy-test/make-service :name "service-2" :marked nil))
         (service-3 (prodigy-test/make-service :name "service-3" :marked t)))
     (should (equal (prodigy-marked-services) (list service-3 service-1))))))


;;;; prodigy-completing-read

(ert-deftest prodigy-completing-read-test/ido ()
  (with-mock
   (mock (ido-completing-read "Input: " '(foo bar)))
   (let ((prodigy-completion-system 'ido))
     (prodigy-completing-read "Input: " '(foo bar)))))

(ert-deftest prodigy-completing-read-test/default ()
  (with-mock
   (mock (completing-read "Input: " '(foo bar)))
   (let ((prodigy-completion-system 'default))
     (prodigy-completing-read "Input: " '(foo bar)))))


;;;; prodigy-read-tag

(ert-deftest prodigy-read-tag-test ()
  (with-sandbox
   (stub prodigy-tags => '(foo bar baz))
   (mock (prodigy-completing-read "tag: " '("foo" "bar" "baz")) => "qux")
   (should (string= (prodigy-read-tag) "qux"))))


;;;; prodigy-buffer-name

(ert-deftest prodigy-buffer-name-test ()
  (with-sandbox
   (let ((service-1 (prodigy-test/make-service :name "foo"))
         (service-2 (prodigy-test/make-service :name "Foo Bar")))
     (should (string= (prodigy-buffer-name service-1) "*prodigy-foo*"))
     (should (string= (prodigy-buffer-name service-2) "*prodigy-foo-bar*")))))


;;;; prodigy-find-service

(ert-deftest prodigy-find-service-test ()
  (with-sandbox
   (let ((service (prodigy-test/make-service :name "foo")))
     (should (equal (prodigy-find-service "foo") service))
     (should-not (prodigy-find-service "bar")))))


;;;; prodigy-service-id

(ert-deftest prodigy-service-id-test ()
  (with-sandbox
   (let ((service (prodigy-test/make-service :name "Foo Bar")))
     (should (eq (prodigy-service-id service) 'foo-bar)))))


;;;; prodigy-find-by-id

(ert-deftest prodigy-find-by-id-test ()
  (with-sandbox
   (let ((service (prodigy-test/make-service :name "Bar")))
     (should-not (prodigy-find-by-id 'foo))
     (should (equal (prodigy-find-by-id 'bar) service)))))


;;;; prodigy-url

(ert-deftest prodigy-url-test ()
  (with-sandbox
   (let ((service-1 (prodigy-test/make-service
                     :name "Bar"
                     :url "http://localhost:1234"
                     :port nil))
         (service-2 (prodigy-test/make-service
                     :name "Bar"
                     :args '("foo" "1234" "bar")
                     :port nil)))
     (should (string= (prodigy-url service-1) "http://localhost:1234"))
     (should (string= (prodigy-url service-2) "http://localhost:1234")))))


;;;; prodigy-discover-initialize


;;;; prodigy-define-default-status-list

(ert-deftest prodigy-define-default-status-list-test ()
  (with-sandbox
   (prodigy-define-default-status-list)
   (should (equal (--map (plist-get it :id) prodigy-status-list)
                  '(failed stopping ready running stopped)))))

(provide 'prodigy-internals-test)

;;; prodigy-internals-test.el ends here
