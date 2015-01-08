;;; prodigy-dependencies-test.el --- Prodigy: Tests for dependency management -*- lexical-binding: t; -*-

(require 'ert-async)
(require 'prodigy)

(ert-deftest prodigy-dependencies-test/init ()
  (let (prodigy-dependent-services prodigy-service-dependencies)
    (prodigy-define-service
      :name "A"
      :depends-on '(("C" running)))
    (prodigy-define-service
      :name "B"
      :depends-on '(("C" running) ("D" running)))
    (prodigy-define-service
      :name "C"
      :depends-on '(("D" running)))
    (prodigy-define-service
      :name "D")

    (should-not (prodigy-service-dependencies-met-p (prodigy-find-service "A")))

    (-each (-map #'prodigy-find-service '("A" "B" "C" "D"))
      'prodigy-dependencies-init)

    (should (equal (cadr (assoc "C" prodigy-dependent-services))
                   '("B" "A")))
    (should (equal (cadr (assoc "D" prodigy-dependent-services))
                   '("C" "B")))
    (should (equal (cadr (assoc "A" prodigy-service-dependencies))
                   '(("C" stopped))))
    (should (equal (cadr (assoc "B" prodigy-service-dependencies))
                   '(("C" stopped) ("D" stopped))))
    (should (equal (cadr (assoc "C" prodigy-service-dependencies))
                   '(("D" stopped))))))

(ert-deftest prodigy-dependencies-test/dependencies-met-p ()
  (prodigy-define-service
    :name "A"
    :depends-on '(("B" ready) ("C" running)))
  (let ((service (prodigy-find-service "A")))
    (let ((prodigy-service-dependencies '(("A" (("B" stopped) ("C" stopped))))))
      (should-not (prodigy-service-dependencies-met-p service)))
    (let ((prodigy-service-dependencies '(("A" (("B" ready) ("C" stopped))))))
      (should-not (prodigy-service-dependencies-met-p service)))
    (let ((prodigy-service-dependencies '(("A" (("B" running) ("C" ready))))))
      (should-not (prodigy-service-dependencies-met-p service)))
    (let ((prodigy-service-dependencies '(("A" (("B" ready) ("C" running))))))
      (should (prodigy-service-dependencies-met-p service)))
    (let ((prodigy-service-dependencies '(("A" (("B" ready) ("C" running))))))
      (should (prodigy-service-dependencies-met-p service))) )

  (prodigy-define-service
    :name "B")
  (should (prodigy-service-dependencies-met-p (prodigy-find-service "B"))))

(ert-deftest prodigy-dependencies-test/update-dependency ()
  (prodigy-define-service
    :name "A"
    :depends-on '(("B" running)))
  (prodigy-define-service
    :name "B")

  (let ((service (prodigy-find-service "A")))
    (prodigy-dependencies-init service)
    (should (equal (cadr (assoc "A" prodigy-service-dependencies))
                   '(("B" stopped))))

    (prodigy-update-dependency service (prodigy-find-service "B") 'running)
    (should (equal (cadr (assoc "A" prodigy-service-dependencies))
                   '(("B" running))))))

(provide 'prodigy-dependencies-test)

;;; prodigy-dependencies-test.el ends here
