(require 'ert)
(require 'ht)

(ert-deftest ht-test-create-non-default-test ()
  (let ((h (ht-create 'eq)))
    (should (equal (hash-table-test h) 'eq))))	

(ert-deftest ht-test-keys ()
  (let ((h (ht-create)))
    (ht-set h "foo" "bar")
    (should (equal (ht-keys h) (list "foo")))))

(ert-deftest ht-test-values ()
  (let ((h (ht-create)))
    (ht-set h "foo" "bar")
    (should (equal (ht-values h) (list "bar")))))

(ert-deftest ht-test-items ()
  (let ((h (ht-create)))
    (ht-set h "key1" "value1")
    (should (equal (ht-items h) '(("key1" "value1"))))))

(ert-deftest ht-test-from-alist ()
  (let* ((alist '(("key1" . "value1")))
         (h (ht-from-alist alist)))
    (should (equal (ht-items h) '(("key1" "value1"))))))

(ert-deftest ht-test-from-alist-masked-values ()
  (let* ((alist '(("key1" . "value1") ("key1" . "value2")))
         (h (ht-from-alist alist)))
    (should (equal (ht-items h) '(("key1" "value1"))))))

(ert-deftest ht-test-from-plist ()
  (let* ((plist '("key1" "value1"))
         (h (ht-from-plist plist)))
    (should (equal (ht-items h) '(("key1" "value1"))))))

(ert-deftest ht-test-to-alist ()
  (let* ((alist '(("key1" . "value1") ("key2" . "value2")))
	 (h (ht-from-alist alist)))
    (should (or (equal (ht-to-alist h) alist)
		(equal (ht-to-alist h) (reverse alist))))))
    

(defun ht-run-tests ()
  (interactive)
  (ert-run-tests-interactively "ht-test-"))
