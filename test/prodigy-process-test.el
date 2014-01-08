(defun make-server-service (&rest args)
  (plist-put args :name "Foo")
  (plist-put args :command "server")
  (plist-put args :cwd prodigy-test/test-path)
  (plist-put args :path (list prodigy-test/test-path))
  (plist-put args :env '(("PORT" "6001")))
  (apply 'prodigy-define-service args)
  (car prodigy-services))

;;;; on-output

(ert-deftest-async prodigy-on-output-test/no-tags (done-starting done-booting done-ready)
  (with-sandbox
   (let ((service (make-server-service
                   :on-output (lambda (service output)
                                (cond ((string= output "Starting...\n")
                                       (funcall done-starting))
                                      ((string= output "Booting...\n")
                                       (funcall done-booting))
                                      ((string= output "Ready!\n")
                                       (funcall done-ready)
                                       (prodigy-stop-service service)))))))
     (prodigy-start-service service))))

(ert-deftest-async prodigy-on-output-test/with-tag (done-service done-tag)
  (with-sandbox
   (prodigy-define-tag
     :name 'tag
     :on-output (lambda (service output)
                  (when (string= output "Ready!\n")
                    (prodigy-stop-service service)
                    (funcall done-tag))))
   (let ((service
          (make-server-service
           :tags '(tag)
           :on-output (lambda (service output)
                        (when (string= output "Ready!\n")
                          (funcall done-service))))))
     (prodigy-start-service service))))


;;;; init-async

(ert-deftest-async prodigy-init-async-test/callbacked ()
  (let (foo)
    (let ((service
           (make-server-service
            :init-async (lambda (done)
                          (setq foo "bar")
                          (funcall done)))))
      (prodigy-start-service service)
      (prodigy-stop-service service)
      (should (string= foo "bar")))))

(ert-deftest-async prodigy-init-async-test/not-callbacked ()
  (should-error
   (let ((prodigy-init-async-timeout 1)
         (service
          (make-server-service
           :init-async (lambda (done)))))
     (prodigy-start-service service)
     (prodigy-stop-service service))))
