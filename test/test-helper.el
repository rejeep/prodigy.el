(require 'f)

(defvar prodigy-test/test-path
  (f-parent (f-this-file)))

(defvar prodigy-test/root-path
  (f-parent prodigy-test/test-path))

(defun make-service (&rest args)
  (let ((plist '(:name "name" :command "command" :cwd "cwd")))
    (append plist args)))

(defmacro with-sandbox (&rest body)
  `(progn
     (setq prodigy-tags nil)
     (setq prodigy-services nil)
     ,@body))

(require 'ert)
(require 'ert-async)
(require 'el-mock)
(require 'cl) ;; for el-mock
(require 'prodigy (f-expand "prodigy" prodigy-test/root-path))
