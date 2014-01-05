
;;;; prodigy-service-port

(ert-deftest prodigy-service-port-test ()
  (should (= (prodigy-service-port '(:port 1234)) 1234))
  (should (= (prodigy-service-port '(:args ("-p" "1234"))) 1234))
  (should (= (prodigy-service-port '(:args ("-p" "12345"))) 12345))
  (should-not (prodigy-service-port '(:args ("-p" "123456"))))
  (should-not (prodigy-service-port ())))


;;;; prodigy-service-command

(ert-deftest prodigy-service-command-test ()
  (let ((prodigy-tags '((:name foo)
                        (:name bar :command "bar")))
        (service-1 '(:name "service-1" :command "cmd"))
        (service-2 '(:name "service-2" :command "cmd" :tags (foo bar)))
        (service-3 '(:name "service-3" :tags (foo bar)))
        (service-4 '(:name "service-4")))
    (should (string= (prodigy-service-command service-1) "cmd"))
    (should (string= (prodigy-service-command service-2) "cmd"))
    (should (string= (prodigy-service-command service-3) "bar"))
    (should-not (prodigy-service-command service-4))))


;;;; prodigy-service-args

(ert-deftest prodigy-service-args-test ()
  (let ((prodigy-tags '((:name foo)
                        (:name bar :args ("bar"))))
        (service-1 '(:name "service-1" :args ("baz")))
        (service-2 '(:name "service-2" :args ("baz") :tags (foo bar)))
        (service-3 '(:name "service-3" :tags (foo bar)))
        (service-4 '(:name "service-4")))
    (should (equal (prodigy-service-args service-1) '("baz")))
    (should (equal (prodigy-service-args service-2) '("baz")))
    (should (equal (prodigy-service-args service-3) '("bar")))
    (should (equal (prodigy-service-args service-4) '()))))


;;;; prodigy-service-cwd

(ert-deftest prodigy-service-cwd-test ()
  (let ((prodigy-tags '((:name foo)
                        (:name bar :cwd "bar")))
        (service-1 '(:name "service-1" :cwd "baz"))
        (service-2 '(:name "service-2" :cwd "baz" :tags (foo bar)))
        (service-3 '(:name "service-3" :tags (foo bar)))
        (service-4 '(:name "service-4")))
    (should (equal (prodigy-service-cwd service-1) "baz"))
    (should (equal (prodigy-service-cwd service-2) "baz"))
    (should (equal (prodigy-service-cwd service-3) "bar"))
    (should (equal (prodigy-service-cwd service-4) '()))))


;;;; prodigy-service-init

(ert-deftest prodigy-service-init-test ()
  (let* ((bar (lambda () "bar"))
         (baz (lambda () "baz"))
         (prodigy-tags `((:name foo)
                         (:name bar :init ,bar)))
         (service-1 `(:name "service-1" :init ,baz))
         (service-2 `(:name "service-2" :init ,baz :tags (foo bar)))
         (service-3 '(:name "service-3" :tags (foo bar)))
         (service-4 '(:name "service-4")))
    (should (equal (prodigy-service-init service-1) baz))
    (should (equal (prodigy-service-init service-2) baz))
    (should (equal (prodigy-service-init service-3) bar))
    (should-not (prodigy-service-init service-4))))


;;;; prodigy-service-init-async

(ert-deftest prodigy-service-init-async-test ()
  (let* ((bar (lambda (done) "bar"))
         (baz (lambda (done) "baz"))
         (prodigy-tags `((:name foo)
                         (:name bar :init-async ,bar)))
         (service-1 `(:name "service-1" :init-async ,baz))
         (service-2 `(:name "service-2" :init-async ,baz :tags (foo bar)))
         (service-3 '(:name "service-3" :tags (foo bar)))
         (service-4 '(:name "service-4")))
    (should (equal (prodigy-service-init-async service-1) baz))
    (should (equal (prodigy-service-init-async service-2) baz))
    (should (equal (prodigy-service-init-async service-3) bar))
    (should-not (prodigy-service-init-async service-4))))


;;;; prodigy-service-stop-signal

(ert-deftest prodigy-service-stop-signal-test ()
  (let ((prodigy-tags '((:name foo)
                        (:name bar :stop-signal sigint)))
        (service-1 '(:name "service-1" :stop-signal int))
        (service-2 '(:name "service-2" :stop-signal int :tags (foo bar)))
        (service-3 '(:name "service-3" :tags (foo bar)))
        (service-4 '(:name "service-4")))
    (should (eq (prodigy-service-stop-signal service-1) 'int))
    (should (eq (prodigy-service-stop-signal service-2) 'int))
    (should (eq (prodigy-service-stop-signal service-3) 'sigint))
    (should-not (prodigy-service-stop-signal service-4))))


;;;; prodigy-service-kill-process-buffer-on-stop

(ert-deftest prodigy-service-kill-process-buffer-on-stop-test ()
  (let ((prodigy-tags '((:name foo)
                        (:name bar :kill-process-buffer-on-stop t)))
        (service-1 '(:name "service-1" :kill-process-buffer-on-stop t))
        (service-2 '(:name "service-2" :kill-process-buffer-on-stop t :tags (foo bar)))
        (service-3 '(:name "service-3" :tags (foo bar)))
        (service-4 '(:name "service-4")))
    (should (prodigy-service-kill-process-buffer-on-stop service-1))
    (should (prodigy-service-kill-process-buffer-on-stop service-2))
    (should (prodigy-service-kill-process-buffer-on-stop service-3))
    (should-not (prodigy-service-kill-process-buffer-on-stop service-4))))


;;;; prodigy-service-path

(ert-deftest prodigy-service-path-test ()
  (let ((prodigy-tags '((:name foo :path ("foo"))
                        (:name bar :path ("bar" "baz"))))
        (service-1 '(:name "service-1" :path ("baz")))
        (service-2 '(:name "service-2" :path ("baz" "qux") :tags (foo bar)))
        (service-3 '(:name "service-3" :tags (foo bar)))
        (service-4 '(:name "service-4")))
    (should (equal (prodigy-service-path service-1) '("baz")))
    (should (equal (prodigy-service-path service-2) '("baz" "qux" "foo" "bar")))
    (should (equal (prodigy-service-path service-3) '("foo" "bar" "baz")))
    (should (equal (prodigy-service-path service-4) '()))))


;;;; prodigy-service-env

(ert-deftest prodigy-service-env-test ()
  (let ((prodigy-tags '((:name foo :env (("FOO" "VALUE")))
                        (:name bar :env (("BAR" "VALUE")
                                         ("BAZ" "VALUE")))))
        (service-1 '(:name "service-1" :env (("BAZ" "VALUE"))))
        (service-2 '(:name "service-2" :env (("BAZ" "VALUE")) :tags (foo bar)))
        (service-3 '(:name "service-3" :tags (foo bar)))
        (service-4 '(:name "service-4")))
    (should (equal (prodigy-service-env service-1) '(("BAZ" "VALUE"))))
    (should (equal (prodigy-service-env service-2) '(("BAZ" "VALUE")
                                                     ("FOO" "VALUE")
                                                     ("BAR" "VALUE"))))
    (should (equal (prodigy-service-env service-3) '(("FOO" "VALUE")
                                                     ("BAR" "VALUE")
                                                     ("BAZ" "VALUE"))))
    (should-not (prodigy-service-env service-4))))


;;;; prodigy-service-url

(ert-deftest prodigy-service-url-test ()
  (let ((prodigy-tags '((:name foo)
                        (:name bar :url "http://bar")))
        (service-1 '(:name "service-1" :url "http://baz"))
        (service-2 '(:name "service-2" :url "http://baz" :tags (foo bar)))
        (service-3 '(:name "service-3" :tags (foo bar)))
        (service-4 '(:name "service-4")))
    (should (equal (prodigy-service-url service-1) "http://baz"))
    (should (equal (prodigy-service-url service-2) "http://baz"))
    (should (equal (prodigy-service-url service-3) "http://bar"))
    (should-not (prodigy-service-url service-4))))
