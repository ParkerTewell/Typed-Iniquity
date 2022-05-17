#lang typed/racket
 
; (: add-t (-> Integer Integer Integer)
; (define (add p1 p2)
;   (let ([p1 #t]) (+ p1 p2)))

; (: add-2 (-> Integer Integer Integer)
; (define (add-2 p1 p2)
;   (let ([p3 2]) (+ (+ p1 p2) p3)))

;; Combined Function 
(: int-or-string (-> Boolean (U String Integer)))
(define (int-or-string b)
    (if b 2 "Hello"))


(: bool-to-int (-> Boolean Integer))
(define (bool-to-int b)
  (string-length (int-or-string b)))

;; Combined Function 
(: int-or-string-2 (-> (U String Integer) Boolean))
(define (int-or-string-2 b)
    (if (string? b) #t #f))


(: int-to-int (-> Integer Integer))
(define (int-to-int b)
  (if (int-or-string-2 b) 1 2))

;; Check if 
(: go-to-else (-> Integer (U String Integer)))
(define (go-to-else b)
    (if (char? b) "Hello" 2))

(: test-1 (-> Integer Integer))
(define (test-1 b)
  (go-to-else b))