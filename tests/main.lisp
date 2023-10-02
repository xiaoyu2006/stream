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
      (ok (equal (stream->list (stream-first-n fibs 10)) '(0 1 1 2 3 5 8 13 21 34)))))
  (testing "prime sieve infinite stream"
    (defun divisible? (x y)
        (zerop (mod x y)))
    (defun sieve (s)
      "Return a stream of sieved non-divisible elements from the given stream."
      (stream-cons (stream-car s)
                   (sieve (stream-filter (lambda (x) (not (divisible? x (stream-car s))))  ; filter out multiples of the first element
                                         (stream-cdr s)))))
    (defun stream-starting-from (n)
      (stream-cons n (stream-starting-from (1+ n))))
    (let ((primes (sieve (stream-starting-from 2))))
      (ok (equal (stream->list (stream-first-n primes 10)) '(2 3 5 7 11 13 17 19 23 29))))))
