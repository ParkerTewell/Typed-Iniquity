#lang racket
(require "../types.rkt")

(define (parse s)
  (match s
    [(list ': f (list-rest '-> xts)) (begin (print xts) (begin (print (map type? (map type-convert (all-but-last xts)))) (print (type? (type-convert (last-element xts))))))]
    [_ "try again"]))