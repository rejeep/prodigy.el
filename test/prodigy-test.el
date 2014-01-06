;;;; prodigy-url

(ert-deftest prodigy-url-test/with-url ()
  (let ((url "http://localhost:1234/secret.html"))
    (should (string= (prodigy-url (make-service :url url)) url))))

(ert-deftest prodigy-url-test/no-url-with-port ()
  (let ((url "http://localhost:6001"))
    (with-mock
     (stub prodigy-service-port => 6001)
     (should (string= (prodigy-url (make-service)) url)))))

(ert-deftest prodigy-url-test/no-url-no-port ()
  (with-mock
   (stub prodigy-service-port)
   (should-not (prodigy-url (make-service)))))
