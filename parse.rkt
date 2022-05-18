#lang racket
(provide parse parse-define parse-e)
(require "ast.rkt" "types.rkt")

;; [Listof S-Expr] -> Prog
(define (parse s)
  (match (parse-untyped s)
    [(UnparsedProg utds tas e) (parse-typed utds tas e)]))
  
;; (Listof UntypedDefn) (Listof TypeAnno) Expr -> Prog
(define (parse-typed utds tas e)
  (match utds
    [(cons utd utds)
     (match (parse-typed utds tas e)
       [(Prog ds e) (Prog (cons (parse-typed-defn utd tas) ds) e)])]
    ['() (Prog '() e)]))

;; UntypedDefn (Listof TypeAnno) -> TypedDefn
(define (parse-typed-defn utd tas)
  (match utd
    [(UntypedDefn f xs e)
     (find-type-defn f xs e tas)]))

(define (find-type-defn f xs e tas)
  (match tas
    [(cons ta tas)
     (match ta
       [(TypeAnno g xts et)
        (if (eq? f g) (TypedDefn f xs e xts et) (find-type-defn f xs e tas))])]
    ['() (let ([xts (generate-list (length xs) '())] [et '()]) (TypedDefn f xs e xts et))]))

(define (generate-list len symbol)
  (if (= len 0) '() (cons symbol (generate-list (- len 1) symbol))))

;; [Listof S-Expr] -> Prog
(define (parse-untyped s)
  (match s
    [(cons (and (cons 'define _) d) s)
     (match (parse-untyped s)
       [(UnparsedProg ds tds e)
        (UnparsedProg (cons (parse-define d) ds) tds e)])]
    [(cons (and (cons ': _) t) s)
      (match (parse-untyped s)
       [(UnparsedProg ds tds e)
        (UnparsedProg ds (cons (parse-type t) tds) e)])]
    [(cons e '()) (UnparsedProg '() '() (parse-e e))]
    [_ (error "program parse error")]))


;; S-Expr -> Defn
;; TypeDefn precedes Defn
;(define (match-type-defn f xs e tds)
;  (match tds
;    [(cons td tds)
;     (match td
;       [(TypeDefn g xts et)
;        (if (eq? g f) (Defn f xs (parse-e e) xts et) (match-type-defn f xs (parse-e e) xts et))])]
;    ['() (Defn f xs (parse-e e) xts et)]))

;; S-Expr -> UntypedDefn
(define (parse-define s)
  (match s
    [(list 'define (list-rest (? symbol? f) xs) e)
     (if (andmap symbol? xs)
         (UntypedDefn f xs (parse-e e))
         (error "parse definition error"))]
    [_ (error "Parse defn error" s)]))

;; S-Expr -> TypeDefn
(define (parse-type t)
  (match t
    [(list ': f (list-rest '-> xts))
     (if (and (andmap type? (map type-convert (all-but-last xts))) (type? (type-convert (last-element xts))))
         (TypeAnno f (map type-convert (all-but-last xts)) (type-convert (last-element xts)))
         (error "parse type definition error"))]
    [_ (error "Parse typedefn error" t)]))

;; S-Expr -> Expr
(define (parse-e s)
  (match s
    [(? integer?)                  (Int s)]
    [(? boolean?)                  (Bool s)]
    [(? char?)                     (Char s)]
    [(? string?)                   (Str s)]
    ['eof                          (Eof)]
    [(? symbol?)                   (Var s)]
    [(list 'quote (list))          (Empty)]
    [(list (? (op? op0) p0))       (Prim0 p0)]
    [(list (? (op? op1) p1) e)     (Prim1 p1 (parse-e e))]
    [(list (? (op? op2) p2) e1 e2) (Prim2 p2 (parse-e e1) (parse-e e2))]
    [(list (? (op? op3) p3) e1 e2 e3)
     (Prim3 p3 (parse-e e1) (parse-e e2) (parse-e e3))]
    [(list 'begin e1 e2)
     (Begin (parse-e e1) (parse-e e2))]
    [(list 'if e1 e2 e3)
     (If (parse-e e1) (parse-e e2) (parse-e e3))]
    [(list 'let (list (list (? symbol? x) e1)) e2)
     (Let x (parse-e e1) (parse-e e2))]
    [(cons (? symbol? f) es)
     (App f (map parse-e es))]
    [_ (error "Parse error" s)]))

(define op0
  '(read-byte peek-byte void))

(define op1
  '(add1 sub1 zero? char? int? write-byte eof-object?
         integer->char char->integer
         box unbox empty? cons? box? car cdr
         vector? vector-length string? string-length))
(define op2
  '(+ - < = cons eq? make-vector vector-ref make-string string-ref))
(define op3
  '(vector-set!))

(define (op? ops)
  (λ (x)
    (and (symbol? x)
         (memq x ops))))
