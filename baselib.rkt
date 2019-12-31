#lang racket/base

(require "ast.rkt")

(provide (all-defined-out))

(define *baselib-types*
  (make-immutable-hash
   (list (cons '%add (Fun 'num (list 'num 'num)))
         (cons '%sub (Fun 'num (list 'num 'num)))
         (cons '%mul (Fun 'num (list 'num 'num)))
         (cons '%div (Fun 'num (list 'num 'num)))
         (cons '%slt (Fun 'num (list 'num 'num)))
         (cons '%equal (Fun 'num (list 'num 'num)))
         (cons '%lesserOrEqual (Fun 'num (list 'num 'num)))
         (cons 'print_int (Fun 'void (list 'num)))
         (cons 'print_bool (Fun 'void (list 'num)))
         (cons 'print_str (Fun 'void (list 'str)))
         (cons 'print_nl  (Fun 'void (list)))
         (cons 'pair (Fun (Pair 'num) (list 'num (Pair 'num))))
         (cons 'head (Fun 'num (list (Pair 'num))))
         (cons 'tail (Fun (Pair 'num) (list (Pair 'num)))))))

(define *baselib*
  (make-immutable-hash
   (list (cons '%add
               (list (Lw 't0 (Mem 'sp 4))
                     (Lw 't1 (Mem 'sp 0))
                     (Add 'v0 't0 't1)))
         (cons '%sub
               (list (Lw 't0 (Mem 'sp 4))
                     (Lw 't1 (Mem 'sp 0))
                     (Sub 'v0 't0 't1)))
                           
         (cons '%mul
               (list (Lw 't0 (Mem 'sp 4))
                     (Lw 't1 (Mem 'sp 0))
                     (Mul 'v0 't0 't1)))
         (cons '%div
               (list (Lw 't0 (Mem 'sp 4))
                     (Lw 't1 (Mem 'sp 0))
                     (Div 'v0 't0 't1)))
         (cons '%slt 
               (list (Lw 't0 (Mem 'sp 4))
                     (Lw 't1 (Mem 'sp 0))
                     (Slt 'v0 't0 't1)))
         (cons '%equal
               (list (Lw 't0 (Mem 'sp 4))
                     (Lw 't1 (Mem 'sp 0))
                     (Slt 't2 't0 't1)
                     (Slt 't3 't1 't0)
                     (Or 'v0 't2 't3)
                     (Xori 'v0 'v0 1)
                     ))

         (cons '%lesserOrEqual
               (list (Lw 't0 (Mem 'sp 4))
                     (Lw 't1 (Mem 'sp 0))
                     (Slt 't2 't0 't1)
                     (Slt 't3 't1 't0)
                     (Or 't3 't2 't3)
                     (Xori 't3 't3 1)
                     (Or 'v0 't2 't3)
                     ))               

         (cons 'print_int
               (list (Lw 'a0 (Mem 'sp 0))
                     (Li 'v0 PRINT_INT)
                     (Syscall)))
         (cons 'print_bool
               (list (Lw 'a0 (Mem 'sp 0))
                     (Li 'v0 PRINT_INT)
                     (Syscall)))
         (cons 'print_str
               (list (Lw 'a0 (Mem 'sp 0))
                     (Li 'v0 PRINT_STRING)
                     (Syscall)))
         (cons 'print_nl
               (list (La 'a0 (Lbl 'newline))
                     (Li 'v0 PRINT_STRING)
                     (Syscall)))
         (cons 'pair
               (list (Jal (Lbl "_pair"))))
         (cons 'head
               (list (Lw 't0 (Mem 'sp 0))
                     (Lw 'v0 (Mem 't0 -4))))
         (cons 'tail
               (list (Lw 't0 (Mem 'sp 0))
                     (Lw 'v0 (Mem 't0 0)))))))

(define *baselib-implem*
  (list (Label "_pair")
        (Li 'a0 8)
        (Li 'v0 SBRK)
        (Syscall)
        (Lw 't0 (Mem 'sp 4))
        (Sw 't0 (Mem 'v0 -4))
        (Lw 't0 (Mem 'sp 0))
        (Sw 't0 (Mem 'v0 0))
        (Jr 'ra)))
