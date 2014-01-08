;;;; prodigy-add-filter

(ert-deftest prodigy-add-filter-test ()
  (prodigy-add-filter :tag 'tag)
  (prodigy-add-filter :name "name")
  (should (equal prodigy-filters '((:name "name")
                                   (:tag tag)))))


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

(ert-deftest prodigy-define-service-test/preserve-process ()
  (prodigy-define-service :name "name" :process "process")
  (prodigy-define-service :name "name")
  (should (equal prodigy-services '((:name "name" :process "process")))))


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


;;;; prodigy-define-status

(ert-deftest prodigy-define-status-test/new-status ()
  (with-sandbox
   (let (prodigy-status-list)
     (prodigy-define-status
       :id 'foo
       :name "Foo"
       :face 'foo-face)
     (let ((status '((:id foo :name "Foo" :face foo-face))))
       (should (equal prodigy-status-list status))))))

(ert-deftest prodigy-define-status-test/override-tag-by-name ()
  (with-sandbox
   (let (prodigy-status-list)
     (prodigy-define-status
       :id 'foo
       :name "Foo"
       :face 'foo-face)
     (prodigy-define-status
       :id 'foo
       :name "Bar"
       :face 'foo-face)
     (let ((status '((:id foo :name "Bar" :face foo-face))))
       (should (equal prodigy-status-list status))))))


;;;; prodigy-set-status

(ert-deftest prodigy-set-status-test/status-defined ()
  (with-sandbox
   (let ((service (make-service)))
     (prodigy-define-status :id 'waiting)
     (prodigy-set-status service 'waiting)
     (should (eq (plist-get service :status) 'waiting)))))

(ert-deftest prodigy-set-status-test/status-not-defined ()
  (with-sandbox
   (let ((service (make-service)))
     (should-error (prodigy-set-status service 'waiting)))))
