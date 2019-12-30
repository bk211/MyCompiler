#lang racket/base

(require racket/match
         "ast.rkt")

(provide mips-print)

(define (format-loc location)
  (match location
    [(Lbl n) (format "~a" n)]
    [(Mem r o) (format "~a($~a)" o r)]
    [(? symbol? r) (format "$~a" r)]))

(define (print-instr instr)
  (match instr
    [(Asciiz n s)       (printf "~a: .asciiz \"~a\"\n" n s)]
    [(Label n)          (printf "~a:\n" n)]
    [(Move d r)         (printf "  move $~a, $~a\n" d r)]
    [(Li d i)           (printf "  li $~a, ~a\n" d i)]
    [(La d l)           (printf "  la $~a, ~a\n" d (format-loc l))]
    [(Sw r l)           (printf "  sw $~a, ~a\n" r (format-loc l))]
    [(Lw r l)           (printf "  lw $~a, ~a\n" r (format-loc l))]
    [(Addi d r i)       (printf "  addi $~a, $~a, ~a\n" d r i)]
    [(Add d r s)        (printf "  add $~a, $~a, $~a\n" d r s)]
    [(Sub d r s)        (printf "  sub $~a, $~a, $~a\n" d r s)]
    [(Mul d r s)        (printf "  mult $~a, $~a\n  mflo $~a\n" r s d)]
    [(Div d r s)        (printf "  div $~a, $~a\n  mflo $~a\n" r s d)]
    [(Lesser d r s)     (printf "  slt $~a, $~a, $~a\n" d r s)]
    [(Sll d r s)        (printf "  sll $~a, $~a, ~a\n" d r s)]
    [(Srl d r s)        (printf "  srl $~a, $~a, ~a\n" d r s)]
    
    [(Ori d r i)        (printf "  ori $~a, $~a, ~a\n" d r i)]
    [(Beq r1 r2 l)      (printf "  beq $~a, $~a, ~a\n" r1 r2 l)]
    [(Bne r1 r2 l)      (printf "  bne $~a, $~a, ~a\n" r1 r2 l)]
    [(Print-label l)    (printf "~a:\n" l)]
    [(Syscall)          (printf "  syscall\n")]
    [(Jal l)            (printf "  jal ~a\n" (format-loc l))]
    [(Jr r)             (printf "  jr $~a\n" r)]))

(define (print-instructions instrs)
  (for-each print-instr instrs))

(define (mips-print asm)
  (printf "  .data\n")
  (print-instructions (Mips-data asm))
  (printf "\n  .text\n  .globl main\n")
  (print-instructions (Mips-text asm)))
