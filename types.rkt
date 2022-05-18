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

;; TODO: Expand Int to Byte and UInt
(define types
  '(Int Bool Char Str Vector Eof Empty Box Cons Void Any))

(define (is-member e list)
  (if (eq? (member e list) #f) #f #t))

(define (all-but-last ele-list)
  (reverse (cdr (reverse ele-list))))

(define (last-element ele-list)
  (car (reverse ele-list)))

(define (append-element lst elem)
  (append lst (list elem)))

;; Only consider single item
;; If pass (list type), check each of them
(define (type? x)
  (match x
    ['() #t]
    [(? symbol?) (is-member x types)]
    [(list 'Listof s) (type? s)]
    [(cons t ts) (and (type? t) (type? ts))]
    [_ #f]))

;; (type-replace-id xts id pt) 
;; (type-rule-out-id xts id pt)

(define (type-remain xts id pt)
  (type-minus (list-ref xts id) pt))

(define (type-replace-id xts id pt)
  (append (reverse (list-tail (reverse xts) (- (length xts) id))) `(,pt) (list-tail xts (+ id 1))))

(define (type-rule-out-id xts id pt)
  (append (reverse (list-tail (reverse xts) (- (length xts) id))) `(,(type-remain xts id pt)) (list-tail xts (+ id 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO: support check (Listof, Cons)

;; Only consider when type-query isn't a list
;; possible type-format:
;; 1. A single type
;; 2. List of type
;; 3. (Listof type)

;; Special:
;; 1. Any contains anything
;; 2. Empty can be contained in Vector/String
;; 3. Listof can be contained in Cons

(define type-convert (λ (p) (match p
                            [(list-rest 'U xts) (map type-convert xts)]
                            ['Integer 'Int]
                            ['Boolean 'Bool]
                            ['String 'Str]
                            ['(Listof Integer) '(Listof Int)]
                            ['(Listof Boolean) '(Listof Bool)]
                            ['(Listof String) '(Listof Str)]                            
                            [_ p])))


(define (type-contain type-format type-query)
  (match type-query
    ['() #t]
    [(list 'Listof tf) (type-contain-single type-format type-query)]
    [(cons type-first type-remainings) (and (type-contain-single type-format type-first) (type-contain type-format type-remainings))]
    [_ (type-contain-single type-format type-query)]))

(define (type-contain-single type-format type-query)
  (match type-format
    ['() #t]
    [(list 'Listof tf)
     (match type-query
       ['Empty #t]
       [(list 'Listof tq) (type-contain tf tq)]
       [_ #f])]
    [(? symbol?) 
     (if (eq? type-format 'Any) #t 
      (if (eq? type-query 'Empty) (match type-format
        ['Cons #t]
        ['Vector #t]
        ['Str #t]
        ['Empty #t]
        [_ #f]) 
        (if (eq? type-format 'Cons) 
          (match type-query
            [(list 'Listof tq) #t]
            ['Cons #t]
            [_ #f]) (eq? type-format type-query))))]
    [(cons type-a type-format) (type-contain-list type-format type-query)]
    [_ #f]))

(define (type-contain-list type-format type-query)
  (match type-format
    ['() #f]
    [(cons type-a type-format) (or (type-contain-single type-a type-format) (type-contain-list type-format type-query))]))

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

;; possible Input:
;; 1. '()
;; 2. 'Any
;; 3. 'symbol
;; 4. (Listof xx)
;; 5. (list Type)

(define (type-add type-or-lst-1 type-or-lst-2)
  (match type-or-lst-1
    ['() type-or-lst-2]
    ['Any 'Any]
    [(? symbol?)
     (match type-or-lst-2
       ['() type-or-lst-1]
       ['Any 'Any]
       [(? symbol?) (if (eq? type-or-lst-1 type-or-lst-2) type-or-lst-1 `(,type-or-lst-1 ,type-or-lst-2))]
       [(list 'Listof tf) `(,type-or-lst-1 ,type-or-lst-2)]
       [(cons type-first types-remaining)
        (if (eq? types-remaining '()) `(,type-or-lst-1 ,type-first)
        (match type-first
          [(? symbol?) (if (type-contain type-first type-or-lst-1) type-or-lst-2 (append `(,type-first) (type-add type-or-lst-1 types-remaining)))]
          [(list 'Listof tf) (append `(,type-first) (type-add type-or-lst-1 types-remaining))]))])]
    [(list 'Listof tf)
     (match type-or-lst-2
       ['() type-or-lst-1]
       ['Any 'Any]
       [(list 'Listof tf2) 
        (if (type-contain tf tf2) type-or-lst-1 
            (if (type-contain tf2 tf) type-or-lst-2 
                `(Listof ,(type-add tf tf2))))]
       [(? symbol?) `(,type-or-lst-1 ,type-or-lst-2)]
       [(cons type-first types-remaining)
        (match type-first
          [(list 'Listof tf2) (if (type-contain type-first type-or-lst-1) type-or-lst-2 (type-add `(Listof ,(type-add tf tf2)) types-remaining))]
          [(? symbol?) (if (eq? types-remaining '()) `(,type-first ,type-or-lst-1) (type-add type-first (type-add type-or-lst-1 types-remaining)))]) 
        ])]
    [(cons type-first types-remaining)
     (match type-or-lst-2
       ['() type-or-lst-1]
       ['Any 'Any]
       [(list 'Listof tf) 
        (match type-first
          [(list 'Listof tf2) 
           (if (type-contain tf tf2) type-or-lst-1
               (if (type-contain tf2 tf) (type-add type-or-lst-2 types-remaining)
                   (type-add `(Listof ,(type-add tf tf2)) types-remaining)))]
          [(? symbol?) (type-add type-first (type-add type-or-lst-2 types-remaining))])]
       [(? symbol?)
        (match type-first
          [(list 'Listof tf2) (append `(,type-first) (type-add type-or-lst-2 types-remaining))]
          [(? symbol?) (if (type-contain type-first type-or-lst-2) (type-or-lst-1) 
                           (if (type-contain type-or-lst-2 type-first) (type-add type-or-lst-2 types-remaining)
                               (append `(,type-first) (type-add type-or-lst-2 types-remaining))))])]
       [(cons type-first-2 types-remaining-2) (type-add types-remaining (type-add type-first type-or-lst-2))])]))