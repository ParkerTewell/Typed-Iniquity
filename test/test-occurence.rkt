#lang racket
(require "../types.rkt")
;(: add-1 (-> Any Integer))
;(define (add1 x) (if (number? x) (add1 x) 0))

(type-minus '(Int Str) 'Int)

(type-replace-id '((Str Int)) 0 'Int)
(type-rule-out-id '((Str Int)) 0 'Int)

(type-contain '(Str Int) 'Int)
(type-contain '(Str Int) 'Str)
(type-contain-single 'Str 'Str)