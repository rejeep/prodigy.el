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


;;;; prodigy-browse

(ert-deftest prodigy-browse-test/no-url ()
  (with-mock
   (stub prodigy-service-at-pos)
   (stub message)
   (not-called browse-url)
   (prodigy-browse)))

(ert-deftest prodigy-browse-test/single-url ()
  (with-mock
   (stub prodigy-service-at-pos => '(:url "http://localhost:3000"))
   (mock (browse-url "http://localhost:3000"))
   (prodigy-browse)))

(ert-deftest prodigy-browse-test/multiple-url ()
  (with-mock
   (stub prodigy-service-at-pos => '(:url ("http://localhost:3000"
                                           "http://localhost:3000/foo")))
   (stub prodigy-completing-read => "http://localhost:3000/foo")
   (mock (browse-url "http://localhost:3000/foo"))
   (prodigy-browse)))
