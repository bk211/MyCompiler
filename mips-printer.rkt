#lang racket/base

(require racket/match
         "ast.rkt" "baselib.rkt")

(provide mips-print)
(define (format-loc location)
  (match location
    [(Lbl n) (format "~a" n)]
    [(Mem r o) (format "~a($~a)" o r)]
    [(? symbol? r) (format "$~a" r)]
    ))

(define uniq-lbl 0)
(define uniq-lbl-e 0)
(define uniq-if 0)

(define (print-instr instr)
  (match instr
    [(Asciiz n s)       (printf "~a: .asciiz \"~a\"\n" n s)]
    [(Label n)          (printf "~a:\n" n)]
    [(Move d r)         (printf "  move $~a, $~a\n" d r)]
    [(Li d i)           (printf "  li $~a, ~a\n" d i)]
    [(La d l)           (printf "  la $~a, ~a\n" d (format-loc l))]
    [(Lb d l)           (printf "  lb $~a, ~a\n" d (format-loc l))]
    [(Sw r l)           (printf "  sw $~a, ~a\n" r (format-loc l))]
    [(Lw r l)           (printf "  lw $~a, ~a\n" r (format-loc l))]
    [(Addi d r i)       (printf "  addi $~a, $~a, ~a\n" d r i)]
    [(Add d r s)        (printf "  add $~a, $~a, $~a\n" d r s)]
    [(Sub d r s)        (printf "  sub $~a, $~a, $~a\n" d r s)]
    [(Mul d r s)        (printf "  mult $~a, $~a\n  mflo $~a\n" r s d)]
    [(Div d r s)        (printf "  div $~a, $~a\n  mflo $~a\n" r s d)]
    [(Slt d r s)        (printf "  slt $~a, $~a, $~a\n" d r s)]
    [(Sll d r s)        (printf "  sll $~a, $~a, ~a\n" d r s)]
    [(Srl d r s)        (printf "  srl $~a, $~a, ~a\n" d r s)]
    [(Or d r i)         (printf "  or $~a, $~a, $~a\n" d r i)]
    [(Ori d r i)        (printf "  ori $~a, $~a, ~a\n" d r i)]
    [(And d r s)        (printf "  and $~a, $~a, $~a\n" d r s)]
    [(Andi d r i)       (printf "  andi $~a, $~a, ~a\n" d r i)]
    [(Xor d r s)        (printf "  xor $~a, $~a, $~a\n" d r s)]
    [(Xori d r i)       (printf "  xori $~a, $~a, ~a\n" d r i)]
    [(Beq r1 r2 l)      (printf "  beq $~a, $~a, ~a\n" r1 r2 (format-loc l))]
    [(Beq-lbl r1 r2)    (printf "  beq $~a, $~a, lbl~a\n" r1 r2 uniq-lbl)]
    [(Beq-if r1 r2)     (printf "  beq $~a, $~a, if~a\n" r1 r2 uniq-if)]
    [(Beq-lbl-e r1 r2)  (printf "  beq $~a, $~a, lbl_e~a\n" r1 r2 uniq-lbl)]
    [(Bne r1 r2 l)      (printf "  bne $~a, $~a, ~a\n" r1 r2 l)]
    [(Print-label)      (printf "lbl~a:\n" uniq-lbl )]
    [(Print-if)         (printf "if~a:\n" uniq-if )]
    [(Print-label-e)    (printf "lbl_e~a:\n" uniq-lbl-e)]
    [(Syscall)          (printf "  syscall\n")]
    [(Jal l)            (printf "  jal ~a\n" (format-loc l))]
    [(Jr r)             (printf "  jr $~a\n" r)]
    [(Jlbl )            (printf "  j lbl~a\n" uniq-lbl)]
    [(Jlbl-e )            (printf "  j lbl_e~a\n" uniq-lbl-e)]
    [(Inc-uniq-lbl)        (set! uniq-lbl (+ uniq-lbl 1))]
    [(Inc-uniq-lbl-e)      (set! uniq-lbl-e (+ uniq-lbl-e 1))]
    [(Inc-uniq-if)              (set! uniq-if (+ uniq-if 1))]
    ))

(define (print-instructions instrs)
  (for-each print-instr instrs))

(define (mips-print asm)
  (printf "  .data\n")
  (print-instructions (Mips-data asm))
  (printf "\n  .text\n  .globl main\n")
  (print-instructions (Mips-text asm)))
