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

(define (initialize-store!)
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

;value-of function
(define (value-of-program pgm)
  (initialize-store!)
  (value-of (parse pgm) (empty-env)))

(define (value-of exp env)
  (cond
    [(and (list-of-statements? exp) (= (length exp) 1))
     (value-of (car exp) env)]
    [(list-of-statements? exp)
     (let ((new-env (car (value-of (car exp) env))))
       (value-of (cdr exp) new-env))]
    [else (value-of-statement exp env)]))

;determines if exp is a list of statements or a statement itself
(define (list-of-statements? exp)
  (if (null? exp)
      true
      (and
       (list? (car exp))
       (list-of-statements? (cdr exp)))))

;determines the value of a single statement
;TODO
(define (value-of-statement exp env)
  (let ((exp-type (car exp)))
    (cond
      [(equal? exp-type 'assign) (assign exp env)]
      [(equal? exp-type 'print) (my-print (cadr exp) env)]
      [(equal? exp-type 'if) (if-exp exp env)]
      [(equal? exp-type 'none) (list env 'None)]
      [(equal? exp-type 'true) (list env #t)]
      [(equal? exp-type 'false) (list env #f)]
      [else exp])))


;value of assign expression
(define (assign exp env)
  (let ((ref (new-ref (cadr (value-of (caddr exp) env)))))
    (let ((var (cadr exp)))
      (cons
       (extend-env var ref env)
       (list 'None)))))


;print function 
(define (my-print exp env) 
  (let ((exp-type (car exp))) 
    (let ((msg 
           (cond 
             [(equal? exp-type 'list) (print-list (cadr exp))] 
             [(equal? exp-type 'true) 'True] 
             [(equal? exp-type 'false) 'False] 
             [(equal? exp-type 'none) 'None] 
             [(equal? exp-type 'num) (cadr exp)] 
             [else (deref (apply-env (cadr exp) env))]))) 
      (begin 
        (print msg) 
        (display "\n") 
        (cons env (list 'None)))))) 

  
;prints a list 
(define (print-list l) 
  exp)

;value of if expression
(define (if-exp exp env)
  (let ((e1 (value-of (cadr exp) env)))
    (cond
      [(or (equal? (cadr e1) #f) (equal? (cadr e1) null)) (value-of (cadddr exp) (car e1))]
      [(equal? (cadr e1) #t) (value-of (caddr exp) (car e1))])))


;test
;(define a (car (parse "if True: return 2; else: return 0;;")))
;(value-of a (empty-env))