(defpackage stream/tests/main
  (:use :cl :stream :rove))
(in-package :stream/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :stream)' in your Lisp.

(deftest test-stream-reduce
  (testing "stream-reduce"
    (let ((s (stream-enum-interval 1 5)))
      (ok (= (stream-reduce #'+ 0 s) 15))
      (ok (= (stream-reduce #'* 1 s) 120)))))

(deftest test-stream-map
  (testing "stream-map"
    (let ((s (stream-enum-interval 1 5)))
      (ok (equal (stream->list (stream-map #'1+ s)) '(2 3 4 5 6)))
      (ok (equal (stream->list (stream-map #'1- s)) '(0 1 2 3 4))))))

(deftest test-infinite-stream
  (testing "infinite fib sequence"
    (defun fibgen (a b)
      (stream-cons a (fibgen b (+ a b))))
    (let ((fibs (fibgen 0 1)))
      (ok (equal (stream->list (stream-first-n fibs 10)) '(0 1 1 2 3 5 8 13 21 34))))))
