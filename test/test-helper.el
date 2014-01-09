;;; test-helper.el --- Prodigy: Test helpers -*- lexical-binding: t; -*-

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

(require 'f)

(defvar prodigy-test/test-path
  (f-parent (f-this-file)))

(defvar prodigy-test/root-path
  (f-parent prodigy-test/test-path))

(defun make-service (&rest args)
  (let ((plist '(:name "name" :command "command" :cwd "cwd")))
    (append plist args)))

(defun make-server-service (&rest args)
  (plist-put args :name "Foo")
  (plist-put args :command "server")
  (plist-put args :cwd prodigy-test/test-path)
  (plist-put args :path (list prodigy-test/test-path))
  (plist-put args :env '(("PORT" "6001")))
  (apply 'prodigy-define-service args)
  (car prodigy-services))

(defmacro with-sandbox (&rest body)
  "Yield BODY in a sandboxed environment."
  `(progn
     (setq prodigy-tags nil)
     (setq prodigy-services nil)
     (setq prodigy-status-list nil)
     (prodigy-define-default-status-list)
     ,@body))

(require 'prodigy (f-expand "prodigy" prodigy-test/root-path))
(require 'ert)
(require 'ert-async)
(require 'el-mock)
(eval-when-compile
  (require 'cl))

(provide 'test-helper)

;;; test-helper.el ends here
