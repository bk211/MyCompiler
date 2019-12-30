#lang racket/base

(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         "helper.rkt")

(provide constants operators get-token)

(define-tokens constants
  (Lnum Lstr Lident Lboolean))

(define-empty-tokens operators
  (Leof
   Lassign
   Lcolon
   LTint LTstr LTboolean
   Llesser LgreaterOrEqual Lequal Lgreater
   Lplus Lminus Lmul Ldiv
   Lopar Lcpar Lcomma Lsep))

(define-lex-abbrev identifier
  (:: alphabetic
      (:* (:or "_" alphabetic numeric))))

(define get-token
  (lexer-src-pos
   [(eof)        (token-Leof)]
   ["\n"         (token-Lsep)]
   [whitespace   (return-without-pos (get-token input-port))]
   [":"          (token-Lcolon)]
   ["int"        (token-LTint)]
   ["str"        (token-LTstr)]
   ["bool"       (token-LTboolean)]
   ["True"       (token-Lboolean 1)]
   ["False"      (token-Lboolean 0)]
   ["true"       (token-Lboolean 1)]
   ["false"      (token-Lboolean 0)]
   ["<"          (token-Llesser)]
   [">"          (token-Lgreater)]
   [">="         (token-LgreaterOrEqual)]
   ["=="         (token-Lequal)]
   ["="          (token-Lassign)]
   ["+"          (token-Lplus)]
   ["-"          (token-Lminus)]
   ["*"          (token-Lmul)]
   ["/"          (token-Ldiv)]
   ["("          (token-Lopar)]
   [")"          (token-Lcpar)]
   [","          (token-Lcomma)]
   [identifier   (token-Lident (string->symbol lexeme))]
   [(:+ numeric) (token-Lnum (string->number lexeme))]
   ["\""         (token-Lstr (read-str input-port))]
   [any-char (err (format "unrecognized character '~a'" lexeme)
                  start-pos)]))

(define read-str
  (lexer
   ["\\\""   (string-append "\"" (read-str input-port))]
   ["\""     ""]
   [any-char (string-append lexeme (read-str input-port))]))
