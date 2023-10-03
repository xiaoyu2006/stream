(defpackage stream/tests/main
  (:use :cl :stream :rove))
(in-package :stream/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :stream)' in your Lisp.

(deftest test-stream-reduce
  (testing "stream-reduce"
    (let ((s (stream-enum-interval 1 5)))
      (ok (= (stream-reduce #'+ 0 s) 15))
      (ok (= (stream-reduce #'* 1 s) 120)))))

(deftest test-stream-filter
  (testing "stream-filter"
    (let ((s (stream-enum-interval 1 5)))
      (ok (equal (stream->list (stream-filter #'oddp s)) '(1 3 5)))
      (ok (equal (stream->list (stream-filter #'evenp s)) '(2 4))))))

(deftest test-stream-map
  (testing "stream-map"
    (let ((s (stream-enum-interval 1 5)))
      (ok (equal (stream->list (stream-map #'1+ s)) '(2 3 4 5 6)))
      (ok (equal (stream->list (stream-map #'1- s)) '(0 1 2 3 4))))))

(deftest test-infinite-stream
  (testing "infinite fibonacci stream"
    (defun fibgen (a b)
      (stream-cons a (fibgen b (+ a b))))
    (let ((fibs (fibgen 0 1)))
      (ok (equal (stream->list (stream-first-n fibs 10)) '(0 1 1 2 3 5 8 13 21 34)))))
  (testing "prime sieve"
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
      (ok (equal (stream->list (stream-first-n primes 10)) '(2 3 5 7 11 13 17 19 23 29)))))
  (testing "pi approximation + convergence acceleration"
    ;; Based on \pi / 4 = 1 - 1/3 + 1/5 - 1/7 + 1/9 - ...
    (defun stream-scale (s factor)
      (stream-map (lambda (x) (* x factor)) s))
    (defun stream-partial-sum (s)
      (let ((fst (stream-car s)) (snd (stream-cdr s)))
        (if (stream-null? snd)
            (stream-cons fst '())
            (stream-cons fst
                         (stream-partial-sum (stream-cons (+ fst (stream-car snd)) (stream-cdr snd)))))))
    (defun pi-summands (n)
      (stream-cons (/ 1.0 n)
                   (stream-map #'- (pi-summands (+ n 2)))))
    (defun pi-approximation ()
      (stream-scale (stream-partial-sum (pi-summands 1)) 4))
    (defun eular-transform (s)
      "Eular transform of a stream. The stream must be infinite."
      (let ((s0 (stream-car s))
            (s1 (stream-car (stream-cdr s)))
            (s2 (stream-car (stream-cdr (stream-cdr s)))))
        (stream-cons (- s2 (/ (* (- s2 s1) (- s2 s1)) (+ s0 (* -2 s1) s2)))
                     (eular-transform (stream-cdr s)))))
    (let ((pi-fast (eular-transform (pi-approximation))))
      (ok (< (abs (- (stream-ref pi-fast 100) pi)) 1.0e-4)))))
