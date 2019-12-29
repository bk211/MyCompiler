#lang racket/base

(provide (all-defined-out))

;;; parsed syntax
(struct Pnil (pos))
(struct Pnum (n pos))
(struct Pstr (s pos))
(struct Pvar (v pos))
(struct Pexpr (expr pos))
(struct Passign (var expr pos type)) 
(struct Pcall (func args pos))
(struct Tint (t pos))
(struct Tstr (t pos))


;;; types
(struct Fun (ret args) #:transparent)
(struct Pair (t)       #:transparent)

;;; AST
(struct Nil ())
(struct Num (n))
(struct Str (s))
(struct Var (v))
(struct Expr (e))
(struct Assign (var expr))
(struct Call (func args))
(struct Data (l))

;;; MIPS
(struct Mips (data text))

; instructions
(struct Asciiz (lbl str))
(struct Label (lbl))
(struct Li (dst imm))
(struct La (dst loc))
(struct Addi (dst reg imm))
(struct Add (dst rg1 rg2))
(struct Sub (dst rg1 rg2))
(struct Mul (dst rg1 rg2))
(struct Div (dst rg1 rg2))
(struct Sw (reg loc))
(struct Lw (reg loc))
(struct Move (dst reg))
(struct Syscall ())
(struct Jal (loc))
(struct Jr (reg))

; locations
(struct Mem (reg offset))
(struct Lbl (name))

; contants
(define PRINT_INT    1)
(define PRINT_STRING 4)
(define SBRK         9)
