#lang racket/base

(require racket/contract racket/stream)

(provide
 (contract-out
  [prefab? predicate/c]
  [in-prefab-struct-fields (prefab? . -> . stream?)]
  [prefab-struct-fields (prefab? . -> . list?)]))

(define (prefab? x)
  (if (prefab-struct-key x) #t #f))

(define (in-prefab-struct-fields x)
  (stream-rest
   (for/stream ([item (struct->vector x)])
               item)))

(define (prefab-struct-fields x)
  (stream->list (in-prefab-struct-fields x)))
