#lang racket/base

(require
 racket/contract racket/fixnum racket/flonum
 "prefab.rkt")

(provide
 (contract-out
  [s-exp? predicate/c]))

;; here, atom refers to the fact that the elments
;; do not need to be inspected to determine if the
;; value satisfies s-exp?
(define (s-exp-atom? x)
  (or (number? x)
      (boolean? x)
      (char? x)
      (symbol? x)
      (string? x)
      (bytes? x)
      (fxvector? x)
      (flvector? x)))

(define (stream-of-s-exps? xs)
  (for/fold ([res #t])
      ([x xs] #:break (not res))
    (s-exp? x)))

(define (s-exp-composite? x)
  (or (and (or (list? x) (vector? x))
           (stream-of-s-exps? x))

      (and (hash? x)
           (stream-of-s-exps? (in-hash-keys x))
           (stream-of-s-exps? (in-hash-values x)))

      (and (prefab? x)
           (stream-of-s-exps? (in-prefab-struct-fields x)))

      (and (pair? x)
           (s-exp? (car x))
           (s-exp? (cdr x)))))

(define (s-exp? x)
  (or (s-exp-atom? x)
      (s-exp-composite? x)))
