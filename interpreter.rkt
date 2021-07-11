#lang racket

(require "parser.rkt")

(define (empty-env) '())

(define (extend-env var ref env)
  (cons var (cons ref env)))

(define (apply-env var env)
  (if (equal? var (car env))
      (cadr env)
      (apply-env var (cddr env))))

