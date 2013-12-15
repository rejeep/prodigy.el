(defun make-service (&rest args)
  (let ((table
         (ht
          (:name "name")
          (:command "command")
          (:cwd "cwd"))))
    (ht-set table :init (plist-get args :init))
    (ht-set table :init-async (plist-get args :init-async))
    table))

(defmacro with-sandbox (&rest body)
  `(with-mock
    (stub start-process => "PROCESS")
    (stub set-process-filter)
    (stub set-process-query-on-exit-flag)
    (stub with-timeout)
    (stub prodigy-service-set)
    (let ((prodigy-init-async-timeout 1))
      ,@body)))


;;;; prodigy-service-port

(ert-deftest prodigy-service-port-test ()
  (should (= (prodigy-service-port (ht (:port 1234))) 1234))
  (should (= (prodigy-service-port (ht (:args '("-p" "1234")))) 1234))
  (should (= (prodigy-service-port (ht (:args '("-p" "12345")))) 12345))
  (should-not (prodigy-service-port (ht (:args '("-p" "123456")))))
  (should-not (prodigy-service-port (ht-create))))


;;;; prodigy-define-service

(ert-deftest prodigy-define-service-test/no-name ()
  (should-error
   (prodigy-define-service
     :command "command"
     :cwd "cwd")))

(ert-deftest prodigy-define-service-test/no-cwd ()
  (should-error
   (prodigy-define-service
     :name "name"
     :command "command")))

(ert-deftest prodigy-define-service-test/no-command ()
  (should-error
   (prodigy-define-service
     :name "name"
     :cwd "cwd")))


;;;; prodigy-start-service

(ert-deftest prodigy-start-service-test/init-no-callback ()
  (with-sandbox
   (mock (start-process) => "PROCESS" :times 1)
   (let (foo)
     (prodigy-start-service
      (make-service
       :init (lambda ()
               (setq foo "bar"))))
     (should (equal foo "bar")))))

(ert-deftest prodigy-start-service-test/init-with-callback-callbacked ()
  (with-sandbox
   (mock (start-process) => "PROCESS" :times 1)
   (let (foo)
     (prodigy-start-service
      (make-service
       :init-async (lambda (callback)
                     (setq foo "bar")
                     (funcall callback))))
     (should (equal foo "bar")))))

(ert-deftest prodigy-start-service-test/init-with-callback-not-callbacked ()
  (should-error
   (with-sandbox
    (not-called start-process)
    (let (foo)
      (prodigy-start-service
       (make-service :init-async (lambda (callback))))))))
