#lang racket/base


(require
 racket/contract racket/generic racket/stream racket/vector
 "core.rkt" "prefab.rkt")

(provide
 (contract-out
  [s-exp-serializable? predicate/c]
  [s-exp-serialize (s-exp-serializable? . -> . s-exp?)])
 gen:s-exp-serializable)

(define (stream-of-s-exp-serializable? xs)
  (for/fold ([res #t])
      ([x xs] #:break (not res))
    (s-exp-serializable? x)))

(define (s-exp-serializable-list? x)
  (and (list? x)
       (stream-of-s-exp-serializable? x)))

(define (s-exp-serializable-vector? x)
  (and (vector? x)
       (stream-of-s-exp-serializable? x)))

(define (s-exp-serializable-hash? x)
  (and (vector? x)
       (stream-of-s-exp-serializable? (in-hash-keys x))
       (stream-of-s-exp-serializable? (in-hash-values x))))

(define (s-exp-serializable-prefab? x)
  (and (prefab? x)
       (stream-of-s-exp-serializable? (in-prefab-struct-fields x))))

(define (s-exp-serializable-pair? x)
  (and (pair? x)
       (s-exp-serializable? (car x))
       (s-exp-serializable? (cdr x))))

(define-generics s-exp-serializable
  (s-exp-serialize s-exp-serializable)
  #:defaults
   ([s-exp-serializable-list?
     (define/generic gen s-exp-serialize)
     (define (s-exp-serialize s-exp-serializable)
       (map gen s-exp-serializable))]

    [s-exp-serializable-vector?
     (define/generic gen s-exp-serialize)
     (define (s-exp-serialize s-exp-serializable)
       (vector-map gen s-exp-serializable))]

    [s-exp-serializable-hash?
     (define/generic gen s-exp-serialize)
     (define (s-exp-serialize s-exp-serializable)
       (for/hash ([(k v) s-exp-serializable])
         (values (gen k) (gen v))))]

    [s-exp-serializable-prefab?
     (define/generic gen s-exp-serialize)
     (define (s-exp-serialize s-exp-serializable)
       (make-prefab-struct
        (prefab-struct-key s-exp-serializable)
        (for/list ([field (in-prefab-struct-fields s-exp-serializable)])
          (gen field))))]

    [s-exp-serializable-pair?
     (define/generic gen s-exp-serialize)
     (define (s-exp-serialize xy)
       (cons (gen (car xy))
             (gen (cdr xy))))])

  #:fast-defaults
  ([s-exp? (define (s-exp-serialize s-exp-serializable) s-exp-serializable)]))


(module+ test

  (require rackunit racket/fixnum racket/flonum)


  (struct my-struct
    (a b)
    #:methods gen:s-exp-serializable
    ((define/generic gen s-exp-serialize)
     (define (s-exp-serialize this)
       `#s(my-struct
           ,(gen (my-struct-a this))
           ,(gen (my-struct-b this))))))

  (check-true (s-exp-serializable? (my-struct 3 4)))
  (check-equal?
   (s-exp-serialize (my-struct 3 4))
   #s(my-struct 3 4))

  (check-false (s-exp? `(x . ,(my-struct 3 4))))
  (check-true (s-exp-serializable? `(x . ,(my-struct 3 4))))

  (check-equal? (s-exp-serialize (list (my-struct 3 4))) '(#s(my-struct 3 4)))


  (check-true (s-exp? #t))
  (check-true (s-exp? 1))
  (check-true (s-exp? #\x))
  (check-true (s-exp? #"abc123"))
  (check-true (s-exp? "abc123"))
  (check-true (s-exp? (fxvector 2 3)))
  (check-true (s-exp? (flvector 2.0 3.0)))
  (check-true (s-exp? 'x))

  (check-true (s-exp? '(x y)))
  (check-true (s-exp? #(x y)))
  (check-true (s-exp? #s(thing x y)))

  (check-false (s-exp? (my-struct 'x 3)))
  (check-false (s-exp? (list (my-struct 'x 3)))))
