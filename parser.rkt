#lang racket/base

(require parser-tools/yacc
         parser-tools/lex
         "lexer.rkt"
         "ast.rkt"
         "helper.rkt")

(provide parse)

(define parse-syntax
  (parser
   (src-pos)
   (tokens constants operators)
   (start prog)
   (end Leof)
   (grammar
    (prog
     [()                (list)]
     [(instr)           (list $1)]
     [(instr Lsep prog) (cons $1 $3)])
    (instr
     [(Lident Lcolon type Lassign expr)     (Passign $1 $5 $4-start-pos $3)] ;;match a assignment
     [(Lif expr Lcolon)                     (Pif $2 $1-start-pos)]           ;;match a condition if
     [(Lwhile expr Lcolon)                  (Pwhile $2 $1-start-pos)]        ;;match a while loop
     [(expr)                                (Pexpr $1 $1-start-pos)])
    (type
     [(LTint)        (Tint "int" $1-start-pos)]     ;; type int
     [(LTstr)        (Tstr "str" $1-start-pos)]     ;; type str
     [(LTboolean)    (Tboolean "boolean" $1-start-pos)] ;; type boolean
     )
    (expr
     [(Lopar Lcpar)                   (Pnil $1-start-pos)]
     [(Lnum)                          (Pnum $1 $1-start-pos)]
     [(Lstr)                          (Pstr $1 $1-start-pos)]
     [(Lident)                        (Pvar $1 $1-start-pos)]
     [(Lboolean)                      (Pboolean $1 $1-start-pos)]
     ;;match arithmetic operation that has '(' and ')' 
     [(Lopar expr Lplus expr Lcpar)   (Pcall '%add (list $2 $4) $2-start-pos)]                     ;; (expr + expr)
     [(Lopar Lplus expr Lcpar)        (Pcall '%add (list (Pnum 0 $2-start-pos) $3) $1-start-pos)]  ;; (+expr)
     [(Lopar expr Lminus expr Lcpar)  (Pcall '%sub (list $2 $4) $4-start-pos)]                     ;; (expr - expr)
     [(Lopar Lminus expr Lcpar)       (Pcall '%sub (list (Pnum 0 $2-start-pos) $3) $1-start-pos)]  ;; (- expr)
     [(Lopar expr Lmul expr Lcpar)    (Pcall '%mul (list $2 $4) $4-start-pos)]                     ;; (expr * expr)
     [(Lopar expr Ldiv expr Lcpar)    (Pcall '%div (list $2 $4) $4-start-pos)]                     ;; (expr / expr)
     ;;match arithmetic operation without bracket 
     [(expr Lplus expr)               (Pcall '%add (list $1 $3) $2-start-pos)]                     ;; expr + expr
     [(Lplus expr)                    (Pcall '%add (list (Pnum 0 $2-start-pos) $2) $1-start-pos)]  ;; +expr
     [(expr Lminus expr)              (Pcall '%sub (list $1 $3) $2-start-pos)]                     ;; +expr
     [(Lminus expr)                   (Pcall '%sub (list (Pnum 0 $2-start-pos) $2) $1-start-pos)]  ;; expr - expr
     [(expr Lmul expr)                (Pcall '%mul (list $1 $3) $2-start-pos)]                     ;; expr * expr
     [(expr Ldiv expr)                (Pcall '%div (list $1 $3) $2-start-pos)]                     ;; expr / expr
     ;;match arithmetic comparison 
     [(expr Llesser expr)             (Pcall '%slt (list $1 $3) $2-start-pos)]   ;; expr < expr
     [(expr Lgreater expr)            (Pcall '%slt (list $3 $1) $2-start-pos)]   ;; expr > expr
     [(expr Lequal expr)              (Pcall '%equal (list $1 $3) $2-start-pos)] ;; expr == expr 
     [(expr LlesserOrEqual expr)      (Pcall '%lesserOrEqual (list $1 $3) $2-start-pos)] ;; expr <= expr
     [(expr LgreaterOrEqual expr)     (Pcall '%lesserOrEqual (list $3 $1) $2-start-pos)] ;; expr >= expr
     [(Lendif)                        (Pcall '%endif (list) $1-start-pos)]       ;; if closing procedure
     [(Lendwhile)                     (Pcall '%endwhile (list) $1-start-pos)]    ;; while closing procedure
     [(Lident Lopar args Lcpar)       (Pcall $1 $3 $1-start-pos)]
      )
    (args
     [()                 (list)]
     [(expr)             (list $1)]
     [(expr Lcomma args) (cons $1 $3)]))
   (precs
   ;; priority settings, multiplication & division > plus & minus > comparison
    (left Llesser LgreaterOrEqual LlesserOrEqual Lequal Lgreater)
    (left Lplus Lminus)
    (left Lmul Ldiv)
    )
   (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
            (err (format "syntax error near ~a~a"
                         (substring (symbol->string tok-name) 1)
                         (if tok-value
                             (format "(~a)" tok-value)
                             ""))
                 start-pos)))))

(define (parse src)
  (port-count-lines! src)
  (parse-syntax (lambda () (get-token src))))
