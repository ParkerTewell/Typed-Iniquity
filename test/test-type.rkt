#lang racket/base
(require "../types.rkt")
(require rackunit)

(check-eq? (type? types 'Int) #t)
(check-eq? (type? types '(Listof Int)) #t)
(check-eq? (type? types '(Int Any)) #t)
(check-eq? (type? types '(Int (Listof Int))) #t)
(check-eq? (type? types '(Int (Listof (Int Char)))) #t)

(check-eq? (type-contain 'Int 'Int) #t)
(check-eq? (type-contain 'Any 'Int) #t)
(check-eq? (type-contain 'Int 'Any) #f)
(check-eq? (type-contain '(Listof Int) 'Any) #f)
(check-eq? (type-contain '(Listof Int) 'Int) #f)
(check-eq? (type-contain '(Listof Any) 'Int) #f)
(check-eq? (type-contain '(Listof Any) '(Listof Int)) #t)
(check-eq? (type-contain 'Any '(Listof Int)) #t)
(check-eq? (type-contain '(Int Char) 'Int) #t)
(check-eq? (type-contain '(Int (Listof (Int Char))) '(Listof Char)) #t)
(check-eq? (type-contain '(Int Char Bool) '(Listof Char)) #f)

;; possible type-origin-list
;; 1. A single type
;; 2. (Listof type)
;; 3. List of type
;; 4. Any

;; possible type-ruled-out
;; 1. A single type 
;; 2. (Listof type)
(type-minus 'Int 'Int)
(type-minus 'Int 'Bool)
(type-minus 'Int '(Listof Int))
(type-minus '(Listof Int) '(Listof Int))
(type-minus '(Listof Any) '(Listof Int))
(type-minus '(Listof Int) 'Int)
(type-minus '(Int Bool) 'Int)
(type-minus '(Int Bool) '(Listof Int))
(type-minus '((Listof Int) Bool) '(Listof Int))
(type-minus '((Listof (Int Bool)) Bool) '(Listof Int))
(type-minus 'Any '(Listof Int))
(type-minus 'Any 'Int)