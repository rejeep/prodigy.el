(require 'f)

(defvar prodigy-test/test-path
  (f-parent (f-this-file)))

(defvar prodigy-test/root-path
  (f-parent prodigy-test/test-path))

(defun make-service (&rest args)
  (let ((plist '(:name "name" :command "command" :cwd "cwd")))
    (plist-put plist :init (plist-get args :init))
    (plist-put plist :init-async (plist-get args :init-async))
    plist))

(defmacro with-sandbox (&rest body)
  `(with-mock
    (stub start-process => "PROCESS")
    (stub set-process-filter)
    (stub set-process-query-on-exit-flag)
    (stub with-timeout)
    (stub prodigy-service-set)
    (let ((prodigy-init-async-timeout 1))
      ,@body)))

(require 'ert)
(require 'el-mock)
(require 'cl) ;; for el-mock
(require 'prodigy (f-expand "prodigy" prodigy-test/root-path))
