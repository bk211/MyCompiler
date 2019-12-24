#lang racket/base

(require racket/match
         "ast.rkt")

(provide simplify)

(define uniq 0)

(define (collect-constant-strings expr)
  (match expr
    [(Nil)
     (cons (Nil)
           (list))]
    [(Num n)
     (cons (Num n)
           (list))]
    [(Str s)
     (set! uniq (add1 uniq))
     (let ([lbl (format "str_~a" uniq)])
       (cons (Data lbl)
             (list (cons lbl s))))]
    [(Var v)
     (cons (Var v)
           (list))]
    [(Call f a)
     (let ([as (map collect-constant-strings a)])
       (cons (Call f (map car as))
             (apply append (map cdr as))))]))

(define (simplify-instr instr)
  (match instr
    [(Assign v e)
     (let ([se (collect-constant-strings e)])
       (cons (Assign v (car se))
             (cdr se)))]
    [(Expr e)
     (let ([se (collect-constant-strings e)])
       (cons (Expr (car se))
             (cdr se)))]))

(define (simplify-prog prog)
  (match prog
    ['()
     (cons (list)
           (list))]
    [(cons i p)
     (let ([si (simplify-instr i)]
           [sp (simplify-prog p)])
       (cons (cons (car si) (car sp))
             (append (cdr si) (cdr sp))))]))

(define (simplify prog)
  (simplify-prog prog))
