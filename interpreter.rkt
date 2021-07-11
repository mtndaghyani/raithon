#lang racket

(require "parser.rkt")

;functions relating to environment
(define (empty-env) '())

(define (extend-env var ref env)
  (cons var (cons ref env)))

(define (apply-env var env)
  (if (equal? var (car env))
      (cadr env)
      (apply-env var (cddr env))))

;functions relating to store
(define (empty-store) '())

(define the-store 'uninitialized)

(define (get-store) the-store)

(define (initialize-store)
  (set! the-store (empty-store)))

(define (new-ref val)
  (let ((next-ref (length the-store)))
    (set! the-store (append the-store (list val)))
    next-ref))

(define (deref ref)
  (list-ref the-store ref))

(define (setref ref val)
  (set! the-store
        (letrec
            ((setref-inner
              (lambda (store1 ref1)
                      (cond
                        [(zero? ref1) (cons val (cdr store1))]
                        [else (cons
                               (car store1)
                               (setref-inner (cdr store1) (- ref1 1)))]))))
          (setref-inner the-store ref))))
  
