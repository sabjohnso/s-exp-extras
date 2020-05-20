#lang racket/base

(require
 racket/contract racket/match
 "core.rkt" "prefab.rkt")

(provide
 current-s-exp-deserializers
 s-exp-deserialize
 (contract-out
  (struct s-exp-deserializer
    ([pred predicate/c]
     [trans (s-exp? . -> . any/c)]))))

(struct s-exp-deserializer
  (pred trans)
  #:property prop:procedure
  (lambda (this data)
    (if ((s-exp-deserializer-pred this) data)
        ((s-exp-deserializer-trans this) data)
        (void))))

(define current-s-exp-deserializers
  (make-parameter '()))

(define (default-s-exp-deserializer x)
  (cond
   [(list? x)
    (for/list ([item x])
      (s-exp-deserialize item))]

   [(vector? x)
    (for/vector ([item x])
      (s-exp-deserialize item))]

   [(prefab? x)
    (apply make-prefab-struct (prefab-struct-key x) (prefab-struct-fields x))]

   [(pair? x)
    (cons (s-exp-deserialize (car x)) (s-exp-deserialize (cdr x)))]

   [(hash? x)
    (for/hash ([(k v) x])
      (values (s-exp-deserialize k)
              (s-exp-deserialize v)))]

   [else x]))

(define (s-exp-deserialize x)
  (let ([res
         (for/fold ([res (void)])
             ([proc (current-s-exp-deserializers)]
              #:break (not (void? res)))
           (proc x))])
    (if (void? res) (default-s-exp-deserializer x)
        res)))
(module+ test
  (require rackunit racket/generic
           "serialize.rkt")

  (struct my-struct
    (a b)
    #:methods gen:s-exp-serializable
    ((define/generic gen s-exp-serialize)
     (define (s-exp-serialize this)
       `#s(my-struct ,(gen (my-struct-a this))
                     ,(gen (my-struct-b this)))))
    #:transparent)

  (define my-struct-deserializer
    (s-exp-deserializer
     (match-lambda [`#s(my-struct ,_ ,_)  #t]
                   [_   #f])
     (match-lambda
       [`#s(my-struct ,a ,b) (my-struct (s-exp-deserialize a) (s-exp-deserialize b))])))

  (check-equal?
   (s-exp-deserialize (s-exp-serialize (my-struct 'x 4)))
   '#s(my-struct x 4))

  (check-true
   ((s-exp-deserializer-pred my-struct-deserializer)
    (s-exp-serialize (my-struct 'x 4))))

  (check-equal?
   (parameterize ([current-s-exp-deserializers  (list my-struct-deserializer)])
     (s-exp-deserialize (s-exp-serialize (my-struct 'x 4))))
   (my-struct 'x 4)))
