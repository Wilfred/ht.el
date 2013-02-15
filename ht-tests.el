(require 'ert)
(require 'ht)

(ert-deftest ht-test-keys ()
  (let ((h (ht-create)))
    (ht-set h "foo" "bar")
    (should (equal (ht-keys h) (list "foo")))))

(ert-run-tests-batch "ht-test-")


