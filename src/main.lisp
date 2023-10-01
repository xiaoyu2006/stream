(defpackage stream
  (:use :cl)
  (:export #:force #:delay #:cached-delay
           #:stream-cons #:stream-car #:stream-cdr #:stream-cache-cons
           #:stream-null?
           #:stream-ref #:stream-first-n #:stream-map #:stream-for-each #:stream-filter #:stream-reduce
           #:stream-display #:stream-enum-interval #:stream->list #:list->stream))
(in-package :stream)

;; Basic stream operations

(defmacro delay (expr)
  "Delay the evaluation of EXPR."
  `(lambda () ,expr))

(defmacro cached-delay (pure-expr)
  "Delay the evaluation of PURE-EXPR. If it's forced the second time, return the cached value.
  Use this when PURE-EXPR is pure and expensive to compute."
  `(let ((cache (cons nil nil)))  ; cache: (is-cached . cached-value)
     (lambda ()
       (if (car cache)
           (cdr cache)
           (setf (car cache) t
                 (cdr cache) ,pure-expr)))))

(defmacro force (delayed)
  "Force the evaluation of DELAYED."
  `(funcall ,delayed))

(defmacro stream-cons (a b)
  "Construct a new stream with head A and tail B."
  `(cons ,a (delay ,b)))

(defmacro stream-cache-cons (a b)
  "Construct a new stream with head A and tail B where B is cached."
  `(cons ,a (cached-delay ,b)))

(defun stream-car (s)
  (car s))

(defun stream-cdr (s)
  (force (cdr s)))

;; Stream-lized list operations

(defun stream-null? (s)
  (null s))

(defun stream-ref (s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(defun stream-first-n (s n)
  (if (or (= n 0) (stream-null? s))
      '()
      (stream-cons (stream-car s)
                   (stream-first-n (stream-cdr s) (- n 1)))))

(defun stream-map (proc s)
  (if (stream-null? s)
      '()
      (stream-cons (funcall proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(defun stream-for-each (proc s)
  (if (stream-null? s)
      'done
      (progn
        (funcall proc (stream-car s))
        (stream-for-each proc (stream-cdr s)))))

(defun stream-filter (pred s)
  (cond ((stream-null? s) '())
        ((funcall pred (stream-car s)) (stream-cons (stream-car s)
                                                    (stream-filter pred (stream-cdr s))))
        (t (stream-filter pred (stream-cdr s)))))

(defun stream-reduce (op initial s)
  (if (stream-null? s)
      initial
      (funcall op (stream-car s)
               (stream-reduce op initial (stream-cdr s)))))

;; Utility functions

(defun stream-display (s)
  (stream-for-each #'print s))

(defun stream-enum-interval (low high)
  (if (> low high)
      '()
      (stream-cons low (stream-enum-interval (+ low 1) high))))

(defun stream->list (s)
  (if (stream-null? s)
      '()
      (cons (stream-car s) (stream->list (stream-cdr s)))))

(defun list->stream (l)
  (if (null l)
      '()
      (stream-cons (car l) (list->stream (cdr l)))))
