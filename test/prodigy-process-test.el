;;; prodigy-process-test.el --- Prodigy: Tests various processes related things -*- lexical-binding: t; -*-

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

;;;; on-output

(ert-deftest-async prodigy-on-output-test/no-tags (done-starting done-booting done-ready)
  (with-sandbox
   (let ((service (make-server-service
                   :on-output (lambda (service output)
                                (cond ((string= output "Starting...\n")
                                       (funcall done-starting))
                                      ((string= output "Booting...\n")
                                       (funcall done-booting))
                                      ((string= output "Ready!\n")
                                       (funcall done-ready)
                                       (prodigy-stop-service service)))))))
     (prodigy-start-service service))))

(ert-deftest-async prodigy-on-output-test/with-tag (done-service done-tag)
  (with-sandbox
   (prodigy-define-tag
     :name 'tag
     :on-output (lambda (service output)
                  (when (string= output "Ready!\n")
                    (prodigy-stop-service service)
                    (funcall done-tag))))
   (let ((service
          (make-server-service
           :tags '(tag)
           :on-output (lambda (service output)
                        (when (string= output "Ready!\n")
                          (funcall done-service))))))
     (prodigy-start-service service))))


;;;; init-async

(ert-deftest-async prodigy-init-async-test/callbacked ()
  (with-sandbox
   (let (foo)
     (let ((service
            (make-server-service
             :init-async (lambda (done)
                           (setq foo "bar")
                           (funcall done)))))
       (prodigy-start-service service)
       (prodigy-stop-service service)
       (should (string= foo "bar"))))))

(ert-deftest-async prodigy-init-async-test/not-callbacked ()
  (with-sandbox
   (should-error
    (let ((prodigy-init-async-timeout 1)
          (service
           (make-server-service
            :init-async (lambda (done)))))
      (prodigy-start-service service)
      (prodigy-stop-service service)))))

(provide 'prodigy-process-test)

;;; prodigy-process-test.el ends here
