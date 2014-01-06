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


;;;; prodigy-define-tag

(ert-deftest prodigy-define-tag-test/new-tag ()
  (prodigy-define-tag
    :name "name"
    :command "foo"
    :cwd "/path/to/name")
  (should (equal prodigy-tags '((:name "name" :command "foo" :cwd "/path/to/name")))))

(ert-deftest prodigy-define-tag-test/override-tag-by-name ()
  (prodigy-define-tag
    :name "name"
    :command "foo"
    :cwd "/path/to/name")
  (prodigy-define-tag
    :name "name"
    :command "bar"
    :cwd "/path/to/name")
  (should (equal prodigy-tags '((:name "name" :command "bar" :cwd "/path/to/name")))))


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
