#lang racket/base

(require racket/match
         "ast.rkt"
         "baselib.rkt"
         "helper.rkt")

(provide compile)

;;;
;;; local helper functions
;;;

(define (stack-push regs)
  (append (list (Addi 'sp 'sp (* -4 (length regs))))
          (let loop ([regs regs])
            (if (null? regs) (list)
                (cons (Sw (car regs) (Mem 'sp (* 4 (- (length regs) 1))))
                      (loop (cdr regs)))))))

(define (stack-pop regs)
  (append (let loop ([regs regs])
            (if (null? regs) (list)
                (cons (Lw (car regs) (Mem 'sp (* 4 (- (length regs) 1))))
                      (loop (cdr regs)))))
          (list (Addi 'sp 'sp (* 4 (length regs))))))

;;;
;;; compiler
;;;

(define (compile-and-push expr env fp-sp)
  (append (compile-expr expr env fp-sp)
          (stack-push (list 'v0))))

(define (compile-expr expr env fp-sp)
  ;;; compile an expression
  ;; expr is the expression
  ;; venv is the environment mapping variable names to their location on the stack, and function names to their code (either inlined or a jump)
  ;; fp-sp is the current distance on the stack between the frame pointer and the stack pointer
  (match expr
    [(Nil)
     (list (Li 'v0 0))]
    [(Num n)
     (list (Li 'v0 n))]
    [(Data l)
     (list (La 'v0 (Lbl l)))]
    [(Var v)
     ;; if the expression is a reference to a variable, load its value from its location on the stack, which is stored in the environment
     (list (Lw 'v0 (hash-ref env v)))]
    [(Call f as)
     ;; if the expression is a call to a function, first compile all its arguments and push them to the stack, then insert the function code from the environment, then pop the stack
     (append
      (apply append (map (lambda (a) (compile-and-push a env fp-sp)) as))
      (hash-ref env f)
      (list (Addi 'sp 'sp (* 4 (length as)))))]
      ))
(define (compile-if cond env fp-sp)
  ;;; compile an expression
  ;; expr is the expression
  ;; venv is the environment mapping variable names to their location on the stack, and function names to their code (either inlined or a jump)
  ;; fp-sp is the current distance on the stack between the frame pointer and the stack pointer
  (match cond
    [(Nil)
     (list (Li 'v0 0)
           (Beq-if 'v0 'zero)
     )]
    [(Num n)
     (list (Li 'v0 n)
           (Beq-if 'v0 'zero)
     )]
    [(Data l)
     (list (La 'v0 (Lbl l)))]
    [(Var v)
     ;; if the expression is a reference to a variable, load its value from its location on the stack, which is stored in the environment
     (list (Lw 'v0 (hash-ref env v))
           (Beq-if 'v0 'zero)
      )]
    [(Call f as)
     ;; if the expression is a call to a function, first compile all its arguments and push them to the stack, then insert the function code from the environment, then pop the stack
     (append
      (apply append (map (lambda (a) (compile-and-push a env fp-sp)) as))
      (hash-ref env f)
      (list (Addi 'sp 'sp (* 4 (length as))))
      (list (Beq-if 'v0 'zero)))]
      )
  
)

(define (compile-while cond env fp-sp)
  ;;; compile an expression
  ;; expr is the expression
  ;; venv is the environment mapping variable names to their location on the stack, and function names to their code (either inlined or a jump)
  ;; fp-sp is the current distance on the stack between the frame pointer and the stack pointer
  (match cond
    [(Nil)
     (list 
           (Print-while )
           (Li 'v0 0)
           (Beq-while 'v0 'zero)
     )]
    [(Num n)
     (list (Print-while )
           (Li 'v0 n)
           (Beq-while 'v0 'zero)
     )]
    [(Data l)
     (list   
           (Print-while)
           (La 'v0 (Lbl l)))]
    [(Var v)
     ;; if the expression is a reference to a variable, load its value from its location on the stack, which is stored in the environment
     (list (Print-while)
           (Lw 'v0 (hash-ref env v))
           (Beq-while 'v0 'zero)
      )]
    [(Call f as)
     ;; if the expression is a call to a function, first compile all its arguments and push them to the stack, then insert the function code from the environment, then pop the stack
     (append
      (list (Print-while))
      (apply append (map (lambda (a) (compile-and-push a env fp-sp)) as))
      (hash-ref env f)
      (list (Addi 'sp 'sp (* 4 (length as))))
      (list (Beq-while 'v0 'zero)))]
      )
  
)


(define (compile-instr instr env fp-sp)
  ;;; compile an instruction
  ;; instr is the instruction
  ;; env and fp-sp are the same as in compile-expr
  (match instr
    [(Assign v e)
     ;; if the instruction is a assignment, first compile and push the assigned value, and then update the environment to point the assigned variable to its location on the stack relative to fp, and update the distance between fp and sp
     (cons (compile-and-push e env fp-sp)
           (cons (hash-set env v (Mem 'fp (- fp-sp 4)))
                 (- fp-sp 4)))]
    [(Expr e)
     ;; if the instruction is an expression, simply compile it
     (cons (compile-expr e env fp-sp)
           (cons env
                 fp-sp))]
    [(If e)
     ;; if the instruction is an if
     (cons (compile-if e env fp-sp)
           (cons env
                 fp-sp))]
    [(While e)
     ;; if the instruction is an while
     (cons (compile-while e env fp-sp)
           (cons env
                 fp-sp))]
))

(define (compile-prog prog env fp-sp)
  (match prog
    ['()
     ;; when the program (which is the body of the main function) ends, pop the local variables from the stack
     (list (Addi 'sp 'sp (- fp-sp)))]
    [(cons i p)
     (let ([ci (compile-instr i env fp-sp)])
       (append (car ci)
               (compile-prog p (cadr ci) (cddr ci))))]))

(define (compile ast)
  (Mips
   (append (list (Asciiz 'newline "\\n"))
           (map (lambda (s) (Asciiz (car s) (cdr s))) (cdr ast)))
   (append *baselib-implem*
           (list (Label 'main)
                 (Addi 'sp 'sp -8)
                 (Sw 'ra (Mem 'sp 0))
                 (Sw 'fp (Mem 'sp 4))
                 (Move 'fp 'sp))
           (compile-prog (car ast) *baselib* 0) ;; init fp-sp at zero
           (list (Lw 'ra (Mem 'sp 0))
                 (Lw 'fp (Mem 'sp 4))
                 (Addi 'sp 'sp 8)
                 (Jr 'ra))) ))
