(defpackage stream/tests/main
  (:use :cl :stream :rove))
(in-package :stream/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :stream)' in your Lisp.

(deftest test-stream-reduce
  (testing "is reduce working"
    (let ((s (stream-enum-interval 1 5)))
      (ok (= (stream-reduce #'+ 0 s) 15))
      (ok (= (stream-reduce #'* 1 s) 120)))))

(deftest test-stream-map
  (testing "is map working"
    (let ((s (stream-enum-interval 1 5)))
      (ok (equal (stream->list (stream-map #'1+ s)) '(2 3 4 5 6)))
      (ok (equal (stream->list (stream-map #'1- s)) '(0 1 2 3 4))))))
