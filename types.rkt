#lang racket
(provide (all-defined-out))

(define imm-shift          3)
(define imm-mask       #b111)
(define ptr-mask       #b111)
(define type-box       #b001)
(define type-cons      #b010)
(define type-vect      #b011)
(define type-str       #b100)
(define int-shift  (+ 1 imm-shift))
(define char-shift (+ 2 imm-shift))
(define type-int      #b0000)
(define mask-int      #b1111)
(define type-char    #b01000)
(define mask-char    #b11111)
(define val-true   #b0011000)
(define val-false  #b0111000)
(define val-eof    #b1011000)
(define val-void   #b1111000)
(define val-empty #b10011000)

(define (bits->value b)
  (cond [(= type-int (bitwise-and b mask-int))
         (arithmetic-shift b (- int-shift))]
        [(= type-char (bitwise-and b mask-char))
         (integer->char (arithmetic-shift b (- char-shift)))]
        [(= b val-true)  #t]
        [(= b val-false) #f]
        [(= b val-eof)  eof]
        [(= b val-void) (void)]
        [(= b val-empty) '()]
        [else (error "invalid bits")]))

(define (imm->bits v)
  (cond [(eof-object? v) val-eof]
        [(integer? v) (arithmetic-shift v int-shift)]
        [(char? v)
         (bitwise-ior type-char
                      (arithmetic-shift (char->integer v) char-shift))]
        [(eq? v #t) val-true]
        [(eq? v #f) val-false]
        [(void? v)  val-void]
        [(empty? v) val-empty]))


(define (imm-bits? v)
  (zero? (bitwise-and v imm-mask)))

(define (int-bits? v)
  (zero? (bitwise-and v mask-int)))

(define (char-bits? v)
  (= type-char (bitwise-and v mask-char)))

(define (cons-bits? v)
  (zero? (bitwise-xor (bitwise-and v imm-mask) type-cons)))

(define (box-bits? v)
  (zero? (bitwise-xor (bitwise-and v imm-mask) type-box)))

(define (vect-bits? v)
  (zero? (bitwise-xor (bitwise-and v imm-mask) type-vect)))

(define (str-bits? v)
  (zero? (bitwise-xor (bitwise-and v imm-mask) type-str)))

(define types
  '(Int Bool Char Str Vector Eof Empty Box Cons Void Any))

(define (is-member e list)
  (if (eq? (member e list) #f) #f #t))

(define (type? types x)
    (match x
      ['() #t]
      [(? symbol?) (is-member x types)]
      [(list 'Listof s) (type? types s)]
      [(cons t ts) (and (type? types t) (type? types ts))]
      [_ #f]))

;; possible type-format:
;; 1. A single type
;; 2. List of type
;; 3. (Listof type)

(define (type-contain type-format type-query)
  (match type-format
    ['() #f]
    [(list 'Listof tf)
      (match type-query
        [(list 'Listof tq) (type-contain tf tq)]
        [_ #f])]
    [(? symbol?) (if (eq? type-format 'Any) #t (if (eq? type-query 'Empty) (or (eq? type-format 'Vector) (eq? type-format 'Str)) (eq? type-format type-query)))]
    [(cons type-a type-format) (or (type-contain type-a type-query) (type-contain type-format type-query))]
    [_ #f]))

;; possible type-origin-list
;; 1. A single type
;; 2. (Listof type)
;; 3. List of type
;; 4. Any

;; possible type-ruled-out
;; 1. A single type 
;; 2. (Listof type)


(define (type-minus type-origin-list type-ruled-out)
  (match type-origin-list
    ['() '()]
    ['Any 
      (match type-ruled-out 
        [(? symbol?) (append (remove* '(Any type-ruled-out) types) '((Listof Any)))]
        [(list 'Listof tf) (append (remove 'Any types) `((Listof ,(type-minus 'Any tf))))]
        [_ (error "type-error" "invalid type: ~a" type-ruled-out)])]
    [(? symbol?) (if (eq? type-origin-list type-ruled-out) '() type-origin-list)]
    [(list 'Listof tf)
      (match type-ruled-out
        [(? symbol?) type-origin-list]
        [(list 'Listof tr) (let ([empty-ele-lst (type-minus tf tr)])
          (if (eq? empty-ele-lst '()) '()
            (if (eq? (length empty-ele-lst) 1)
              (list 'Listof (list-ref empty-ele-lst 0))
              (list 'Listof empty-ele-lst))))]
        [_ (error "type-error" "invalid type: ~a" type-ruled-out)])]
    [(cons type-first types-remaining) 
      (let ([ele-listele-lst (type-minus type-first type-ruled-out)])
        (match ele-listele-lst
          [(? symbol? ele-listele-lst) (append `(,ele-listele-lst) (type-minus types-remaining type-ruled-out))]
          [(list 'Listof _) (append `(,ele-listele-lst) (type-minus types-remaining type-ruled-out))]
          [_ (append ele-listele-lst (type-minus types-remaining type-ruled-out))]))]))