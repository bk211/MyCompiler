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
   Llesser LgreaterOrEqual LlesserOrEqual Lequal Lgreater
   Lplus Lminus Lmul Ldiv
   Lstrlen
   Lif Lelse
   Lendif Lendelse
   Lwhile Lendwhile
   Lobra Lcbra
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
   ["if"         (token-Lif)]
   ["else"       (token-Lelse)]
   ["endif"      (token-Lendif)]
   ["endelse"    (token-Lendelse)]
   ["while"      (token-Lwhile)]
   ["endwhile"   (token-Lendwhile)]
   [">"          (token-Lgreater)]
   ["<"          (token-Llesser)]
   [">="         (token-LgreaterOrEqual)]
   ["<="         (token-LlesserOrEqual)]
   ["=="         (token-Lequal)]
   ["="          (token-Lassign)]
   ["+"          (token-Lplus)]
   ["-"          (token-Lminus)]
   ["*"          (token-Lmul)]
   ["/"          (token-Ldiv)]
   ["("          (token-Lopar)]
   [")"          (token-Lcpar)]
   ["["          (token-Lobra)]
   ["]"          (token-Lcbra)]
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
