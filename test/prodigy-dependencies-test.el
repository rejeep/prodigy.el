;;; prodigy-dependencies-test.el --- Prodigy: Tests for dependency management -*- lexical-binding: t; -*-

(require 'ert-async)
(require 'prodigy)

(ert-deftest prodigy-dependencies-test/one-level-dependency ()
  (with-sandbox
   (prodigy-define-service
     :name "A"
     :depends-on '(("B" . running))
     :stop-signal 'kill)
   (prodigy-define-service
     :name "B"
     :stop-signal 'kill)

   (let ((service-a (prodigy-find-service "A"))
         (service-b (prodigy-find-service "B")))
     (progn
       (should-not (prodigy-service-will-start-p service-a))
       (should-not (prodigy-service-will-start-p service-b)))

     (progn ;; Start parent service
       (prodigy-start-service service-a
                              (lambda ()
                                (should (prodigy-service-started-p service-a))
                                (should (prodigy-service-started-p service-b))))

       (should (prodigy-service-waiting-p service-a))
       (should (prodigy-service-will-start-p service-a))
       (should-not (prodigy-service-will-stop-p service-a))

       (should (prodigy-service-started-p service-b)))

     (progn ;; Set child to ready
       (prodigy-set-status service-b 'ready)

       (should (prodigy-service-waiting-p service-a))
       (should (prodigy-service-will-start-p service-a))
       (should-not (prodigy-service-will-stop-p service-a))

       (should (prodigy-service-started-p service-b)))

     (progn ;; Set child to running
       (prodigy-set-status service-b 'running)

       (should (prodigy-service-started-p service-a))
       (should (prodigy-service-started-p service-b))

       (should-not (prodigy-service-will-start-p service-a))
       (should-not (prodigy-service-will-start-p service-b)))

     (progn ;; Stop child
       (prodigy-stop-service service-b
                             (lambda ()
                               (should-not (prodigy-service-started-p service-a))
                               (should-not (prodigy-service-started-p service-b)))))))
  )

(ert-deftest prodigy-dependencies-test/two-level-dependency ()
  (with-sandbox
   (prodigy-define-service
     :name "A"
     :depends-on '(("B" . ready))
     :stop-signal 'kill)
   (prodigy-define-service
     :name "B"
     :depends-on '(("C" . running))
     :stop-signal 'kill)
   (prodigy-define-service
     :name "C"
     :stop-signal 'kill)

   (let ((service-a (prodigy-find-service "A"))
         (service-b (prodigy-find-service "B"))
         (service-c (prodigy-find-service "C")))
     (progn
       (should-not (prodigy-service-will-start-p service-a))
       (should-not (prodigy-service-will-start-p service-b))
       (should-not (prodigy-service-will-start-p service-c)))

     (progn
       (prodigy-start-service service-a
                              (lambda ()
                                (should (prodigy-service-started-p service-a))
                                (should (prodigy-service-started-p service-b))
                                (should (prodigy-service-started-p service-c))))

       (should (prodigy-service-waiting-p service-a))
       (should (prodigy-service-waiting-p service-b))
       (should-not (prodigy-service-waiting-p service-c)))

     (progn
       (prodigy-set-status service-c 'ready)

       (should (prodigy-service-waiting-p service-a))
       (should (prodigy-service-waiting-p service-b))
       (should-not (prodigy-service-waiting-p service-c)))

     (progn
       (prodigy-set-status service-c 'running)

       (should (prodigy-service-waiting-p service-a))
       (should-not (prodigy-service-waiting-p service-b))
       (should-not (prodigy-service-waiting-p service-c)))

     (progn
       (prodigy-set-status service-b 'running)

       (should (prodigy-service-waiting-p service-a))
       (should-not (prodigy-service-waiting-p service-b))
       (should-not (prodigy-service-waiting-p service-c)))

     (progn
       (prodigy-set-status service-b 'ready)

       (should-not (prodigy-service-waiting-p service-a))
       (should-not (prodigy-service-waiting-p service-b))
       (should-not (prodigy-service-waiting-p service-c)))

     (progn
       (prodigy-stop-service service-b
                             (lambda ()
                               (should-not (prodigy-service-started-p service-a))
                               (should-not (prodigy-service-started-p service-b))
                               (should (prodigy-service-started-p service-c))))

       (prodigy-stop-service service-c
                             (lambda ()
                               (should-not (prodigy-service-started-p service-a))
                               (should-not (prodigy-service-started-p service-b))
                               (should-not (prodigy-service-started-p service-c))))))))

(ert-deftest prodigy-dependencies-test/two-level-forked-dependency ()
  (with-sandbox
   (prodigy-define-service
     :name "A"
     :depends-on '(("B" . running)
                   ("D" . running))
     :stop-signal 'kill)
   (prodigy-define-service
     :name "B"
     :depends-on '(("C" . running))
     :stop-signal 'kill)
   (prodigy-define-service
     :name "C"
     :stop-signal 'kill)
   (prodigy-define-service
     :name "D"
     :stop-signal 'kill)

   (let ((service-a (prodigy-find-service "A"))
         (service-b (prodigy-find-service "B"))
         (service-c (prodigy-find-service "C"))
         (service-d (prodigy-find-service "D")))
     (progn
       (should-not (prodigy-service-will-start-p service-a))
       (should-not (prodigy-service-will-start-p service-b))
       (should-not (prodigy-service-will-start-p service-c))
       (should-not (prodigy-service-will-start-p service-d)))

     (progn
       (prodigy-start-service service-a
                              (lambda ()
                                (should (prodigy-service-started-p service-a))
                                (should (prodigy-service-started-p service-b))
                                (should (prodigy-service-started-p service-c))
                                (should (prodigy-service-started-p service-d))

                                (should-not (prodigy-service-waiting-p service-a))
                                (should-not (prodigy-service-waiting-p service-b))
                                (should-not (prodigy-service-waiting-p service-c))
                                (should-not (prodigy-service-waiting-p service-d))

                                (progn
                                  (prodigy-stop-service service-d
                                                        (lambda ()
                                                          (should-not (prodigy-service-started-p service-a))
                                                          (should (prodigy-service-started-p service-b))
                                                          (should (prodigy-service-started-p service-c))
                                                          (should-not (prodigy-service-started-p service-d)))))))

       (should (prodigy-service-waiting-p service-a))
       (should (prodigy-service-waiting-p service-b))
       (should-not (prodigy-service-waiting-p service-c))
       (should-not (prodigy-service-waiting-p service-d)))

     (progn
       (prodigy-set-status service-c 'running)

       (should (prodigy-service-waiting-p service-a))
       (should-not (prodigy-service-waiting-p service-b))
       (should-not (prodigy-service-waiting-p service-c))
       (should-not (prodigy-service-waiting-p service-d)))

     (progn
       (prodigy-set-status service-d 'running)))))

(provide 'prodigy-dependencies-test)
;;; prodigy-dependencies-test.el ends here
