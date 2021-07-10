#lang racket



(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(define raithon-lexer
           (lexer
            
            ((:or (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9)))) (token-NUMBER (string->number lexeme)))
            ("=" (token-EQUAL))
            ("==" (token-EQEQUAL))
            ("<" (token-LESS))
            (">" (token-MORE))
            ("+" (token-PLUS))
            ("-" (token-MINUS))
            ("*" (token-STAR))
            ("/" (token-SLASH))
            ("**" (token-DOUBLESTAR))
            ("pass" (token-PASS))
            ("break" (token-BREAK))
            ("continue" (token-CONTINUE))
            ("return" (token-RETURN))
            ("global" (token-GLOBAL))
            ("def" (token-DEF))
            ("if" (token-IF))
            ("else" (token-ELSE))
            ("for" (token-FOR))
            ("in" (token-IN))
            ("or" (token-OR))
            ("and" (token-AND))
            ("not" (token-NOT))
            ("True" (token-TRUE))
            ("False" (token-FALSE))
            ("None" (token-NONE))
            (";" (token-SEMI))
            ("(" (token-LPAR))
            (")" (token-RPAR))
            (":" (token-COLON))
            ("," (token-COMMA))
            ("[" (token-LSQB))
            ("]" (token-RSQB))
            ((:+ (:or alphabetic numeric "_")) (token-ID lexeme))
            (whitespace (raithon-lexer input-port))
            ((eof) (token-EOF))))

(define-tokens literals (ID NUMBER))
(define-empty-tokens operators (EQUAL EQEQUAL LESS MORE PLUS MINUS STAR SLASH DOUBLESTAR))
(define-empty-tokens keywords (PASS BREAK CONTINUE RETURN
                               GLOBAL DEF IF ELSE FOR IN
                               OR AND NOT TRUE FALSE NONE))
(define-empty-tokens seperators (EOF SEMI LPAR RPAR COLON COMMA LSQB RSQB))






;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this raithon-lexer (open-input-string "for i in [1, 2, 3]:\n global x; if i > 2: x = x + i; else x = x - i;")))



(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)
(my-lexer)

