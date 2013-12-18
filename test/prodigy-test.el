
;;;; prodigy-service-port

(ert-deftest prodigy-service-port-test ()
  (should (= (prodigy-service-port '(:port 1234)) 1234))
  (should (= (prodigy-service-port '(:args ("-p" "1234"))) 1234))
  (should (= (prodigy-service-port '(:args ("-p" "12345"))) 12345))
  (should-not (prodigy-service-port '(:args ("-p" "123456"))))
  (should-not (prodigy-service-port ())))


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


;;;; prodigy-define-service

(ert-deftest prodigy-define-service-test/new-service ()
  (prodigy-define-service
    :name "name"
    :command "foo"
    :cwd "/path/to/name")
  (should (equal prodigy-services '((:name "name" :command "foo" :cwd "/path/to/name")))))

(ert-deftest prodigy-define-service-test/override-service-by-name ()
  (prodigy-define-service
    :name "name"
    :command "foo"
    :cwd "/path/to/name")
  (prodigy-define-service
    :name "name"
    :command "bar"
    :cwd "/path/to/name")
  (should (equal prodigy-services '((:name "name" :command "bar" :cwd "/path/to/name")))))
