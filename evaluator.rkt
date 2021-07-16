#lang racket
(require "interpreter.rkt")

(define (evaluate input)
  (value-of-program
   (file->string input)))

(evaluate "test.txt")

