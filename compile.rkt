#lang racket
(provide (all-defined-out))
(require "ast.rkt" "types.rkt" "compile-ops.rkt" a86/ast)

;; Registers used
(define rax 'rax) ; return
(define rbx 'rbx) ; heap
(define rsp 'rsp) ; stack
(define rdi 'rdi) ; arg

;; type CEnv = [Listof Variable]

;; Prog -> Asm
(define (compile p)
  (begin 
    (compile-check-prog p)
    (match p
      [(Prog ds e)  
      (prog (externs)
            (Global 'entry)
            (Label 'entry)
            (Mov rbx rdi)   ; recv heap pointer
            (compile-e e '())
            (Ret)
            (compile-defines ds)
            (Label 'raise_error_align)
            pad-stack
            (Call 'raise_error))])
    )
  )

;; check Prog
(define (compile-check-prog p)
  (match p
    [(Prog ds e)
      (begin
        (compile-check-defs ds p)
        (compile-check-e e p)
      )]))

(define (compile-check-e e p)
  (check-expression-type 'main '() e '() '() p))

(define (compile-check-defs ds p)
  (match ds
    [(cons d ds) (begin (compile-check-def d p) (compile-check-defs ds p))]
    ['() "check completed!"]))

(define (compile-check-def d p)
  (match d
    [(TypedDefn f xs e xts et)
      (check-expression-type f xs e xts et p)]))

(define (check-prim f e et at)
  (if (check-prim? et at) (void) (error 'type-error "In function.expression: [~a.~a], expected: ~a; actual: ~a" f e et at)))

(define (check-prim? et at) (type-contain et at))

(define (check-prim-list f e et ats) (if (check-prim-list? et ats) (void) (error 'type-error "In function.expression: [~a.~a], expected: ~a; actual type list: ~a" f e et ats)))

(define (check-prim-list? et ats) (check-prim-list-idx? et ats 0))

(define (check-prim-list-idx? et ats idx)
  (if (= idx (length ats)) 
    #f  
    (if (check-prim? et (list-ref ats idx)) #t (check-prim-list-idx? et ats (+ idx 1)))))

(define (check-op-return-type f e et at)
  (if (check-op-return-type? et at) (void) (error 'type-error "In function.expression: [~a.~a], expected op return type: ~a; actual: ~a" f e et at)))

(define (check-op-return-type? et at) (check-prim? et at))

(define (check-expressions-type f xs es xts et prog)
  (if (check-expressions-type? xs es xts et prog) (void) (error 'type-error "In function.expressions: [~a.~a], expected op return type: ~a" f es et)))

(define (check-expressions-type? xs es xts et prog)
  (match es
    ['() #f]
    [(cons e es) (if (check-expression-type? xs e xts et prog) #t (check-expressions-type? xs es xts et prog))]))

(define (check-expression-types f xs e xts ets prog)
  (if (check-expression-types? xs e xts ets prog) (void) (error 'type-error "In function.expressions: [~a.~a], expected op return type: ~a" f e ets)))

(define (check-expression-types? xs e xts ets prog)
  (match ets
    ['() #f]
    [(cons et ets) (if (check-expression-type? xs e xts et prog) #t (check-expression-types? xs e xts ets prog))]))


(define (check-if-clauses f xs xts e2 e3 et p id prog)
  (match p
    ['char?        (check-if-clauses-type f xs xts e2 e3 et 'Char id prog) ]
    ['eof-object?  (check-if-clauses-type f xs xts e2 e3 et 'Eof id prog) ]
    ['empty?       (check-if-clauses-type f xs xts e2 e3 et 'Empty id prog) ]
    ['box?         (check-if-clauses-type f xs xts e2 e3 et 'Box id prog) ]
    ['cons?        (check-if-clauses-type f xs xts e2 e3 et 'Cons id prog) ]
    ['vector?      (check-if-clauses-type f xs xts e2 e3 et 'Vector id prog) ]
    ['string?      (check-if-clauses-type f xs xts e2 e3 et 'Str id prog) ]))

(define (check-if-clauses? xs xts e2 e3 et p id prog)
  (match p
    ['char?        (check-if-clauses-type? xs xts e2 e3 et 'Char id prog) ]
    ['eof-object?  (check-if-clauses-type? xs xts e2 e3 et 'Eof id prog) ]
    ['empty?       (check-if-clauses-type? xs xts e2 e3 et 'Empty id prog) ]
    ['box?         (check-if-clauses-type? xs xts e2 e3 et 'Box id prog) ]
    ['cons?        (check-if-clauses-type? xs xts e2 e3 et 'Cons id prog) ]
    ['vector?      (check-if-clauses-type? xs xts e2 e3 et 'Vector id prog) ]
    ['string?      (check-if-clauses-type? xs xts e2 e3 et 'Str id prog) ]))

(define (check-if-clauses-type f xs xts e2 e3 et pt id prog)
  (if (type-contain pt (list-ref xts id)) 
    (check-expression-type f xs e2 xts et prog)
    (if (type-contain (list-ref xts id) pt)
      (match (type-remain xts id pt)
        ['() (check-expression-type f xs e2 (type-replace-id xts id pt) et prog)]
        [_ (begin (check-expression-type f xs e2 (type-replace-id xts id pt) et prog) (check-expression-type f xs e3 (type-rule-out-id xts id pt) et prog))]) 
      (check-expression-type f xs e3 xts et prog))))

(define (check-if-clauses-type? xs xts e2 e3 et pt id prog)
  (if (type-contain pt (list-ref xts id)) 
    (check-expression-type? xs e2 xts et prog)
    (if (type-contain (list-ref xts id) pt)
      (match (type-remain xts id pt)
        ['() (check-expression-type? xs e2 (type-replace-id xts id pt) et prog)]
        [_ (begin (check-expression-type? xs e2 (type-replace-id xts id pt) et prog) (check-expression-type? xs e3 (type-rule-out-id xts id pt) et prog))]) 
      (check-expression-type? xs e3 xts et prog))))

(define (get-if-clauses-expression-type xs xts e2 e3 p id prog)
  (match p
    ['char?        (get-if-clauses-expression-type-internal xs xts e2 e3 'Char id prog) ]
    ['eof-object?  (get-if-clauses-expression-type-internal xs xts e2 e3 'Eof id prog) ]
    ['empty?       (get-if-clauses-expression-type-internal xs xts e2 e3 'Empty id prog) ]
    ['box?         (get-if-clauses-expression-type-internal xs xts e2 e3 'Box id prog) ]
    ['cons?        (get-if-clauses-expression-type-internal xs xts e2 e3 'Cons id prog) ]
    ['vector?      (get-if-clauses-expression-type-internal xs xts e2 e3 'Vector id prog) ]
    ['string?      (get-if-clauses-expression-type-internal xs xts e2 e3 'Str id prog) ]))

(define (get-if-clauses-expression-type-internal xs xts e2 e3 pt id prog)
  (if (type-contain (list-ref xts id) pt)
    (match (type-remain xts id pt)
      ['() (get-expression-type xs xts e3 prog)]
      [_ (type-add (get-expression-type xs (type-replace-id xts id pt) e2 prog) (get-expression-type xs (type-rule-out-id xts id pt) e3 prog))])
    (get-expression-type xs xts e3 prog)))

(define (get-func-from-prog g prog)
    (match prog
      [(Prog ds e) (get-func-from-defs g ds)]))

(define (get-func-from-defs g ds)
  (match ds
    ['() (error 'function-undefined "Function ~a is not defined" g)]
    [(cons d ds)
      (match d
        [(TypedDefn f xs e xts et) (if (eq? f g) d (get-func-from-defs g ds))])]))

(define (check-func-params g xs es xts g-xts prog)
  (match es
    [(? list?) (check-func-params-idx g xs es xts g-xts prog 0)]
    [_ (check-single-func-params g xs es xts g-xts prog)]))

(define (check-single-func-params g xs es xts g-xts prog)
  (check-expression-type g xs es xts g-xts prog))

(define (check-func-params-idx g xs es xts g-xts prog idx)
  (if (eq? (length es) idx) (void) 
  (begin (check-expression-type g xs (list-ref es idx) xts (list-ref g-xts idx) prog)
    (check-func-params-idx g xs es xts g-xts prog (+ idx 1)))))

(define (get-expression-type xs xts e prog)
  (match e
    [(Int i)            'Int]
    [(Bool b)           'Bool]
    [(Char c)           'Char]
    [(Eof)              'Eof]
    [(Empty)            'Empty]
    [(Var x)            (let ([id-or-f (index-of xs x)]) (if (not id-or-f) (error 'undefined-error "In expression: [~a], undefined variable: ~a" e x) (list-ref xts id-or-f))) ]
    ; [(Var x)            (let ([xt (list-ref xts (index-of x xs))]) (if (type-contain et xt) (void) (error 'type-error "expected: ~a; actual: ~a" et xt))]       ;; type check
    [(Str s)            'Str]
    [(Prim0 p)
      (match p
        ['void          'Void]
        ['read-byte     '(Int Eof)]
        ['peek-byte     '(Int Eof)])] 
    [(Prim1 p e0)        
      (match p
        ['add1          'Int]  ; Int -> Int
        ['sub1          'Int]  ; Int -> Int
        ['zero?         'Bool] ; Int -> Bool
        ['char?         'Bool] ; Any -> Bool
        ['char->integer 'Int]  ; Char -> Int
        ['integer->char 'Char] ; Int -> Char
        ['eof-object?   'Bool] ; Any -> Bool
        ['write-byte    'Void] ; Int -> Void
        ['box           'Box] ; Any -> Box
        ['unbox  
          (match e0
            [(Prim1 'box b) (get-expression-type xs xts b prog)]
            [_ (error 'type-error "In expression: [~a], expression ~a is not a Box!" e e0)])]  ; Box -> Any
        ['car 
          (match e0
            [(Prim2 'cons a b) (get-expression-type xs xts a prog)]
            [_ (error 'type-error "In expression: [~a], expression ~a is not a Cons!" e e0)])] ; Cons -> Any
        ['cdr
          (match e0
            [(Prim2 'cons a b) (get-expression-type xs xts b prog)]
            [_ (error 'type-error "In expression: [~a], expression ~a is not a Cons!" e e0)])] ; Cons -> Any
        ['empty?        'Bool] ; Any -> Bool 
        ['box?          'Bool] ; Any -> Bool 
        ['cons?         'Bool] ; Any -> Bool 
        ['vector?       'Bool] ; Any -> Bool 
        ['string?       'Bool] ; Any -> Bool 
        ['vector-length 'Int] ; Vector -> Int 
        ['string-length 'Int] ; Str -> Int 
        )]        ;; type check
    [(Prim2 p e1 e2)    
      (match p
        ['+     'Int] ; Int Int -> Int
        ['-     'Int] ; Int Int -> Int
        ['<     'Bool] ; Int Int -> Bool
        ['=     'Bool] ; Int Int -> Bool
        ['cons  'Cons] ; Any Any -> Cons
        ['eq?   'Bool]  ; Any Any -> Bool
        ['make-vector 'Vector]
        ['vector-ref  (match e1
          [(Prim2 p2 e21 e22)
          #:when (= p2 'make-vector)
          (get-expression-type xs xts e22 prog)]
          [_ (error 'unsupported error "vector-ref is only used after make-vector")])]
        ['make-string 'String]
        ['string-ref  'Char]
      )]
    [(Prim3 p e1 e2 e3) 
      (match p
        ['vector-set! 'Void])] ; Vector Int Any -> Void
    [(If e1 e2 e3)
        (match e1
          [(Bool #t)  (get-expression-type xs xts e2 prog)]
          [(Bool #f)  (get-expression-type xs xts e3 prog)]
          [(Prim1 p e0) ;; match predicate '(char? eof-object? empty? box? cons? vector? string?)
            ;#:when (is-member p op1-predicates)
            (=> exit)
            (match e0
              [(Var x) (if (not (is-member p op1-predicates)) (exit) (let ([id-or-f (index-of xs x)]) 
                (if (not id-or-f) (error 'undefined-error "In expression: ~a, undefined variable: ~a" e x) 
                (get-if-clauses-expression-type xs xts e2 e3 p id-or-f prog))))]
              [_ (exit)]
            )]          
          [(? (check-expression-type? xs e1 xts 'Bool prog)) (type-add (get-expression-type xs xts e2 prog) (get-expression-type xs xts e3 prog))]
          [_ (get-expression-type xs xts e2 prog)])]      ;; type check
    [(Begin e1 e2)      (get-expression-type xs xts e2 prog)]      ;; type check
    [(Let x e1 e2)      
      (if (is-member x xs) (get-expression-type xs xts e2 prog)
        (get-expression-type (append-element xs x) (append-element xts (get-expression-type xs xts e1 prog)) e2 prog))]      ;; type check
    [(App g es)  (let ([g-func (get-func-from-prog g prog)]) (match g-func
      [(TypedDefn g g-xs g-xts g-e g-et) g-et]))]))


(define (check-expression-type f xs e xts et prog)
  (match e
    [(Int i)            (check-prim f e et 'Int)]
    [(Bool b)           (check-prim f e et 'Bool)]
    [(Char c)           (check-prim f e et 'Char)]
    [(Eof)              (check-prim f e et 'Eof)]
    [(Empty)            (check-prim f e et 'Empty)]
    [(Var x)            (let ([id-or-f (index-of xs x)]) (if (not id-or-f) (error 'undefined-error "In function.expression: [~a.~a], undefined variable: ~a" f e x) (check-prim f e et (list-ref xts id-or-f))))]
    ; [(Var x)            (let ([xt (list-ref xts (index-of x xs))]) (if (type-contain et xt) (void) (error 'type-error "expected: ~a; actual: ~a" et xt))]       ;; type check
    [(Str s)            (check-prim f e et 'Str)]
    [(Prim0 p)
      (match p
        ['void          (check-prim f e et 'Void)]
        ['read-byte     (check-prim-list f e et '(Int Eof))]
        ['peek-byte     (check-prim-list f e et '(Int Eof))])] 
    [(Prim1 p e0)        
      (match p
        ['add1          (begin (check-op-return-type f e et 'Int) (check-expression-type f xs e0 xts 'Int prog))]  ; Int -> Int
        ['sub1          (begin (check-op-return-type f e et 'Int) (check-expression-type f xs e0 xts 'Int prog))]  ; Int -> Int
        ['zero?         (begin (check-op-return-type f e et 'Bool) (check-expression-type f xs e0 xts 'Int prog))] ; Int -> Bool
        ['char?         (begin (check-op-return-type f e et 'Bool) (check-expression-type f xs e0 xts '() prog))] ; Any -> Bool
        ['char->integer (begin (check-op-return-type f e et 'Int) (check-expression-type f xs e0 xts 'Char prog))]  ; Char -> Int
        ['integer->char (begin (check-op-return-type f e et 'Char) (check-expression-type f xs e0 xts 'Int prog))] ; Int -> Char
        ['eof-object?   (begin (check-op-return-type f e et 'Bool) (check-expression-type f xs e0 xts '() prog))] ; Any -> Bool
        ['write-byte    (begin (check-op-return-type f e et 'Void) (check-expression-type f xs e0 xts 'Int prog))] ; Int -> Void
        ['box           (begin (check-op-return-type f e et 'Box) (check-expression-type f xs e0 xts '() prog))] ; Any -> Box
        ['unbox         (check-expression-type f xs e0 xts 'Box prog)]  ; Box -> Any
        ['car           (check-expression-types f xs e0 xts `(Cons (Listof ,et)) prog)] ; Cons -> Any
        ['cdr           (check-expression-types f xs e0 xts `(Cons ,et (Listof ,et)) prog)] ; Cons -> Any
        ['empty?        (begin (check-op-return-type f e et 'Bool) (check-expression-type f xs e0 xts '() prog))] ; Any -> Bool 
        ['box?          (begin (check-op-return-type f e et 'Bool) (check-expression-type f xs e0 xts '() prog))] ; Any -> Bool 
        ['cons?         (begin (check-op-return-type f e et 'Bool) (check-expression-type f xs e0 xts '() prog))] ; Any -> Bool 
        ['vector?       (begin (check-op-return-type f e et 'Bool) (check-expression-type f xs e0 xts '() prog))] ; Any -> Bool 
        ['string?       (begin (check-op-return-type f e et 'Bool) (check-expression-type f xs e0 xts '() prog))] ; Any -> Bool 
        ['vector-length (begin (check-op-return-type f e et 'Int) (check-expression-type f xs e0 xts 'Vector prog))] ; Vector -> Int 
        ['string-length (begin (check-op-return-type f e et 'Int) (check-expression-type f xs e0 xts 'Str prog))] ; Str -> Int 
        )]        ;; type check
    [(Prim2 p e1 e2)    
      (match p
        ['+   (begin (check-op-return-type f e et 'Int) (begin (check-expression-type f xs e1 xts 'Int prog) (check-expression-type f xs e2 xts 'Int prog)))] ; Int Int -> Int
        ['-   (begin (check-op-return-type f e et 'Int) (begin (check-expression-type f xs e1 xts 'Int prog) (check-expression-type f xs e2 xts 'Int prog)))] ; Int Int -> Int
        ['<   (begin (check-op-return-type f e et 'Bool) (begin (check-expression-type f xs e1 xts 'Int prog) (check-expression-type f xs e2 xts 'Int prog)))] ; Int Int -> Bool
        ['=   (begin (check-op-return-type f e et 'Bool) (begin (check-expression-type f xs e1 xts 'Int prog) (check-expression-type f xs e2 xts 'Int prog)))] ; Int Int -> Bool
        ['cons  (match et
          ['() (begin (check-expression-type f xs e1 xts '() prog) (check-expression-type f xs e2 xts '() prog))]
          [(list Listof tf) (begin (check-expression-type f xs e1 xts tf prog) (check-expression-type f xs e2 xts et prog))]
          ['Cons (begin (check-expression-type f xs e1 xts '() prog) (check-expression-type f xs e2 xts '() prog))]
          [_ (error 'type-error "In function.expression [~a.~a], expected: ~a; actual: ~a" f e et 'Cons)])] ; Any Any -> Cons
        ['eq?   (begin (check-op-return-type f e et 'Bool) (begin (check-expression-type f xs e1 xts '() prog) (check-expression-type f xs e2 xts '() prog)))]  ; Any Any -> Bool
        ['make-vector (begin (check-op-return-type f e et 'Vector) (begin (check-expression-type f xs e1 xts 'Int prog) (check-expression-type f xs e2 xts '() prog)))]  ; Int Any -> Vector
        ['vector-ref  (begin (check-expression-type f xs e1 xts 'Vector prog) (check-expression-type f xs e2 xts 'Int prog))] ; Vector Int -> Any
        ['make-string (begin (check-op-return-type f e et 'Str) (begin (check-expression-type f xs e1 xts 'Int prog) (check-expression-type f xs e2 xts 'Char prog)))]  ; Int Char -> Str
        ['string-ref  (begin (check-op-return-type f e et 'Char) (begin (check-expression-type f xs e1 xts 'Str prog) (check-expression-type f xs e2 xts 'Int prog)))] ; Str Int -> Char
      )]
    [(Prim3 p e1 e2 e3) 
      (match p
        ['vector-set! (begin (check-op-return-type f e et 'Void) 
        (begin (check-expression-type f xs e1 xts 'Vector prog) 
        (begin (check-expression-type f xs e2 xts 'Int prog) (check-expression-type f xs e3 xts '() prog))))])] ; Vector Int Any -> Void
    [(If e1 e2 e3)
      (begin (check-expression-type f xs e1 xts '() prog) 
        (match e1
          [(Bool #t)  (check-expression-type f xs e2 xts et prog)]
          [(Bool #f)  (check-expression-type f xs e3 xts et prog)]
          [(Prim1 p e0) ;; match predicate '(char? eof-object? empty? box? cons? vector? string?)
            ;#:when (is-member p op1-predicates)
            (=> exit)
            (match e0
              [(Var x) (if (not (is-member p op1-predicates)) (exit) (let ([id-or-f (index-of xs x)]) 
                (if (not id-or-f) (error 'undefined-error "In expression: ~a, undefined variable: ~a" e x) 
                (check-if-clauses f xs xts e2 e3 et p id-or-f prog))))]
              [_ (exit)]
            )]          
          [_ (if (check-expression-type? xs e1 xts 'Bool prog) (check-expressions-type f xs `(,e2 ,e3) xts et prog) (check-expression-type f xs e2 xts et prog))]))]      ;; type check
    [(Begin e1 e2)      (begin (check-expression-type f xs e1 xts '() prog) (check-expression-type f xs e2 xts et prog))]      ;; type check
    [(Let x e1 e2)      
      (if (is-member x xs) 
        (begin (check-expression-type f xs e1 xts (list-ref xts (index-of xs x)) prog) 
              (check-expression-type f xs e2 xts et prog))
        (check-expression-type f (append-element xs x) e2 (append-element xts (get-expression-type xs xts e1 prog)) et prog))]      ;; type check
    [(App g es)  (let ([g-func (get-func-from-prog g prog)]) (match g-func
      [(TypedDefn g g-xs g-e g-xts g-et)
        (begin (check-prim f e et g-et)
              (check-func-params g xs es xts g-xts prog))]))]))

;; TODO: fix
(define (check-expression-type? xs e xts et prog)
  (match e
    [(Int i)            (check-prim? et 'Int)]
    [(Bool b)           (check-prim? et 'Bool)]
    [(Char c)           (check-prim? et 'Char)]
    [(Eof)              (check-prim? et 'Eof)]
    [(Empty)            (check-prim? et 'Empty)]
    [(Var x)            (check-prim? et (list-ref xts (index-of xs x)))]
    [(Str s)            (check-prim? et 'Str)]
    [(Prim0 p)
      (match p
        ['void          (check-prim? et 'Void)]
        ['read-byte     (check-prim-list? '(Int Eof))]
        ['peek-byte     (check-prim-list? '(Int Eof))])] 
    [(Prim1 p e0)        
      (match p
        ['add1          (begin (check-op-return-type? et 'Int) (check-expression-type? xs e0 xts 'Int prog))]  ; Int -> Int
        ['sub1          (begin (check-op-return-type? et 'Int) (check-expression-type? xs e0 xts 'Int prog))]  ; Int -> Int
        ['zero?         (begin (check-op-return-type? et 'Bool) (check-expression-type? xs e0 xts 'Int prog))] ; Any -> Bool
        ['char?         (check-op-return-type? et 'Bool)] ; Any -> Bool
        ['char->integer (begin (check-op-return-type? et 'Int) (check-expression-type? xs e0 xts 'Char prog))]  ; Char -> Int
        ['integer->char (begin (check-op-return-type? et 'Char) (check-expression-type? xs e0 xts 'Int prog))] ; Int -> Char
        ['eof-object?   (check-op-return-type? et 'Bool)] ; Any -> Bool
        ['write-byte    (begin (check-op-return-type? et 'Void) (check-expression-type? xs e0 xts 'Int prog))] ; Int -> Void
        ['box           (check-op-return-type? et 'Box)] ; Any -> Box
        ['unbox         (check-expression-type? xs e0 xts 'Box prog)]  ; Box -> Any
        ['car           (check-expression-type? xs e0 xts 'Cons prog)] ; Cons -> Any
        ['cdr           (check-expression-type? xs e0 xts 'Cons prog)] ; Cons -> Any
        ['empty?        (check-op-return-type? et 'Bool)] ; Any -> Bool 
        ['box?          (check-op-return-type? et 'Bool)] ; Any -> Bool 
        ['cons?         (check-op-return-type? et 'Bool)] ; Any -> Bool 
        ['vector?       (check-op-return-type? et 'Bool)] ; Any -> Bool 
        ['string?       (check-op-return-type? et 'Bool)] ; Any -> Bool 
        ['vector-length (begin (check-op-return-type? et 'Int) (check-expression-type? xs e0 xts 'Vector prog))] ; Vector -> Int 
        ['string-length (begin (check-op-return-type? et 'Int) (check-expression-type? xs e0 xts 'Str prog))] ; Str -> Int 
        )]        ;; type check
    [(Prim2 p e1 e2)    
      (match p
        ['+   (begin (check-op-return-type? et 'Int) (begin (check-expression-type? xs e1 xts 'Int prog)  (check-expression-type? xs e2 xts 'Int prog)))] ; Int Int -> Int
        ['-   (begin (check-op-return-type? et 'Int) (begin (check-expression-type? xs e1 xts 'Int prog)  (check-expression-type? xs e2 xts 'Int prog)))] ; Int Int -> Int
        ['<   (begin (check-op-return-type? et 'Bool) (begin (check-expression-type? xs e1 xts 'Int prog) (check-expression-type? xs e2 xts 'Int prog)))] ; Int Int -> Bool
        ['=   (begin (check-op-return-type? et 'Bool) (begin (check-expression-type? xs e1 xts 'Int prog) (check-expression-type? xs e2 xts 'Int prog)))] ; Int Int -> Bool
        ['cons  (check-op-return-type? et 'Cons)] ; Any Any -> Cons
        ['eq?   (check-op-return-type? et 'Bool)]  ; Any Any -> Bool
        ['make-vector (begin (check-op-return-type? et 'Vector) (check-expression-type? xs e1 xts 'Int prog))]  ; Int Any -> Vector
        ['vector-ref  (begin (check-expression-type? xs e1 xts 'Vector prog) (check-expression-type? xs e2 xts 'Int prog))] ; Vector Int -> Any
        ['make-string (begin (check-op-return-type? et 'Str) (begin (check-expression-type? xs e1 xts 'Int prog) (check-expression-type? xs e2 xts 'Char prog)))]  ; Int Char -> Str
        ['string-ref  (begin (check-op-return-type? et 'Char) (begin (check-expression-type? xs e1 xts 'Str prog) (check-expression-type? xs e2 xts 'Int prog)))] ; Str Int -> Char
      )]
    [(Prim3 p e1 e2 e3) 
      (match p
        ['vector-set! (begin (check-op-return-type? et 'Void) (begin (check-expression-type? xs e1 xts 'Vector prog) (check-expression-type? xs e2 xts 'Int prog)))])] ; Vector Int Any -> Void
;;;;;;;;;;;;;; TODO: fix the following ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    
    [(If e1 e2 e3)
      (begin (check-expression-type? xs e1 xts '() prog) 
        (match e1
          [(Bool #t)  (check-expression-type? xs e2 xts et prog)]
          [(Bool #f)  (check-expression-type? xs e3 xts et prog)]
          [(Prim1 p e0) ;; match predicate '(char? eof-object? empty? box? cons? vector? string?)
            ;#:when (is-member p op1-predicates)
            (=> exit)
            (match e0
              [(Var x)
               (if (not (is-member p op1-predicates)) (exit) (let ([id-or-f (index-of xs x)]) 
                (if (not id-or-f) (error 'undefined-error "In expression: ~a, undefined variable: ~a" e x) 
                (check-if-clauses? xs xts e2 e3 et p id-or-f prog))))]
              [_ (exit)]
            )]          
          [_ (if (check-expression-type? xs e1 xts 'Bool prog) (check-expressions-type? xs `(,e2 ,e3) xts et prog) (check-expression-type? xs e2 xts et prog))]))]      ;; type check
    [(Begin e1 e2)      (begin (check-expression-type? xs e1 xts '() prog) (check-expression-type? xs e2 xts et prog))]      ;; type check
    [(Let x e1 e2)      
      (if (is-member x xs) 
        (begin (check-expression-type? xs e1 xts (list-ref xts (index-of xs x)) prog) 
              (check-expression-type? xs e2 xts et prog))
        (check-expression-type? (append-element xs x) e2 (append-element xts (get-expression-type xs xts e1 prog)) et prog))]      ;; type check
    [(App g es) (let ([g-func (get-func-from-prog g prog)]) (match g-func
      [(TypedDefn g g-xs g-xts g-e g-et) (check-prim? et g-et)]))]))





(define (externs)
  (seq (Extern 'peek_byte)
       (Extern 'read_byte)
       (Extern 'write_byte)
       (Extern 'raise_error)))

;; [Listof Defn] -> Asm
(define (compile-defines ds)
  (match ds
    ['() (seq)]
    [(cons d ds)
     (seq (compile-define d)
          (compile-defines ds))]))

;; Defn -> Asm
(define (compile-define d)
  (match d
    [(TypedDefn f xs e xts et)
     (seq (Label (symbol->label f))
          (compile-e e (reverse xs))
          (Add rsp (* 8 (length xs))) ; pop args
          (Ret))]))

;; Expr CEnv -> Asm
;; check if c matches the type of xt
(define (compile-e e c)
  (match e
    [(Int i)            (compile-value i)]
    [(Bool b)           (compile-value b)]
    [(Char c)           (compile-value c)]
    [(Eof)              (compile-value eof)]
    [(Empty)            (compile-value '())]
    [(Var x)            (compile-variable x c)]       ;; type check
    [(Str s)            (compile-string s)]
    [(Prim0 p)          (compile-prim0 p c)]          ;; type check
    [(Prim1 p e)        (compile-prim1 p e c)]        ;; type check
    [(Prim2 p e1 e2)    (compile-prim2 p e1 e2 c)]    ;; type check
    [(Prim3 p e1 e2 e3) (compile-prim3 p e1 e2 e3 c)] ;; type check
    [(If e1 e2 e3)      (compile-if e1 e2 e3 c)]      ;; type check
    [(Begin e1 e2)      (compile-begin e1 e2 c)]      ;; type check
    [(Let x e1 e2)      (compile-let x e1 e2 c)]      ;; type check
    [(App f es)         (compile-app f es c)]))       ;; type check

;; Value -> Asm
(define (compile-value v)
  (seq (Mov rax (imm->bits v))))

;; Id CEnv -> Asm
(define (compile-variable x c)
  (let ((i (lookup x c)))
    (seq (Mov rax (Offset rsp i)))))

;; String -> Asm
(define (compile-string s)
  (let ((len (string-length s)))
    (if (zero? len)
        (seq (Mov rax type-str))
        (seq (Mov rax len)
             (Mov (Offset rbx 0) rax)
             (compile-string-chars (string->list s) 8)
             (Mov rax rbx)
             (Or rax type-str)
             (Add rbx
                  (+ 8 (* 4 (if (odd? len) (add1 len) len))))))))

;; [Listof Char] Integer -> Asm
(define (compile-string-chars cs i)
  (match cs
    ['() (seq)]
    [(cons c cs)
     (seq (Mov rax (char->integer c))
          (Mov (Offset rbx i) 'eax)
          (compile-string-chars cs (+ 4 i)))]))

;; Op0 CEnv -> Asm
(define (compile-prim0 p c)
  (compile-op0 p))

;; Op1 Expr CEnv -> Asm
(define (compile-prim1 p e c)
  (seq (compile-e e c)
       (compile-op1 p)))

;; Op2 Expr Expr CEnv -> Asm
(define (compile-prim2 p e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons #f c))
       (compile-op2 p)))

;; Op3 Expr Expr Expr CEnv -> Asm
(define (compile-prim3 p e1 e2 e3 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons #f c))
       (Push rax)
       (compile-e e3 (cons #f (cons #f c)))
       (compile-op3 p)))

;; Expr Expr Expr CEnv -> Asm
(define (compile-if e1 e2 e3 c)
  (let ((l1 (gensym 'if))
        (l2 (gensym 'if)))
    (seq (compile-e e1 c)
         (Cmp rax val-false)
         (Je l1)
         (compile-e e2 c)
         (Jmp l2)
         (Label l1)
         (compile-e e3 c)
         (Label l2))))

;; Expr Expr CEnv -> Asm
(define (compile-begin e1 e2 c)
  (seq (compile-e e1 c)
       (compile-e e2 c)))

;; Id Expr Expr CEnv -> Asm
(define (compile-let x e1 e2 c)
  (seq (compile-e e1 c)
       (Push rax)
       (compile-e e2 (cons x c))
       (Add rsp 8)))

;; Id [Listof Expr] CEnv -> Asm
;; The return address is placed above the arguments, so callee pops
;; arguments and return address is next frame
(define (compile-app f es c)
  (let ((r (gensym 'ret)))
    (seq (Lea rax r)
         (Push rax)
         (compile-es es (cons #f c))
         (Jmp (symbol->label f))
         (Label r))))

;; [Listof Expr] CEnv -> Asm
(define (compile-es es c)
  (match es
    ['() '()]
    [(cons e es)
     (seq (compile-e e c)
          (Push rax)
          (compile-es es (cons #f c)))]))

;; Id CEnv -> Integer
(define (lookup x cenv)
  (match cenv
    ['() (error "undefined variable:" x)]
    [(cons y rest)
     (match (eq? x y)
       [#t 0]
       [#f (+ 8 (lookup x rest))])]))

;; Symbol -> Label
;; Produce a symbol that is a valid Nasm label
(define (symbol->label s)
  (string->symbol
   (string-append
    "label_"
    (list->string
     (map (λ (c)
            (if (or (char<=? #\a c #\z)
                    (char<=? #\A c #\Z)
                    (char<=? #\0 c #\9)
                    (memq c '(#\_ #\$ #\# #\@ #\~ #\. #\?)))
                c
                #\_))
         (string->list (symbol->string s))))
    "_"
    (number->string (eq-hash-code s) 16))))
