#lang racket

(require "parser.rkt")

;functions relating to environment
#|
(define (empty-env) '())

(define (extend-env var ref env)
  (cons var (cons ref env)))

(define (apply-env var env)
  (if (equal? var (car env))
      (cadr env)
      (apply-env var (cddr env))))
|#

;another environment implementation
(define empty-env
  (lambda () (list 'empty-env)))

(define extend-env
  (lambda (var ref env)
    (list 'extend-env var ref env)))

(define extend-env-rec
  (lambda (pname vars body env)
    (list 'extend-env-rec pname vars body env)))

(define (extend-env* vars refs env)
  (if (empty? vars)
      env
      (extend-env* (cdr vars) (cdr refs)
                   (extend-env (car vars) (car refs) env))))
(define apply-env
  (lambda (env search-var)
    (cond
      ((eqv? (car env) 'empty-env)
       (report-no-binding-found search-var))
      ((eqv? (car env) 'extend-env)
       (let ((saved-var (cadr env))
             (saved-ref (caddr env))
             (saved-env (cadddr env)))
         (if (eqv? search-var saved-var)
             saved-ref
             (apply-env saved-env search-var))))
      ((eqv? (car env) 'extend-env-rec)
       (let ((pname (cadr env))
             (vars (caddr env))
             (body (cadddr env))
             (saved-env (cadr (cdddr env))))
         (if (eqv? search-var pname)
             (new-ref (procedure vars body env))
             (apply-env saved-env search-var))))       
      (else
       (report-invalid-env env)))))
(define report-no-binding-found
  (lambda (search-var)
    (display "No binding")))
(define report-invalid-env
  (lambda (env)
    (display "Bad environment")))

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
; functions related to proc

(define proc?
  (lambda (val)
    (procedure? val)))

(define procedure
  (lambda (vars stmts env)
    (lambda (refs)
      (value-of stmts (extend-env* vars refs env)))))

(define apply-procedure
  (lambda (proc1 val)
    (proc1 val)))

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
      [(equal? exp-type 'for) (for-exp exp env)]
      [(equal? exp-type 'proc) (def-exp exp env)]
      [(equal? exp-type 'list) (list env exp)]
      [(equal? exp-type 'var) (var exp env)]
      [(equal? exp-type 'none) (list env 'None)]
      [(equal? exp-type 'true) (list env 'True)]
      [(equal? exp-type 'false) (list env 'False)]
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
  (let ((msg (get-content exp env)))
    (if (void? msg)
        (cons env (list 'None))
        (begin
          (print msg)
          (display "\n")
          (cons env (list 'None))))))

;extracts the printable content of an abstact syntax object
(define (get-content exp env)
  (let ((exp-type (car exp)))
    (cond
      [(equal? exp-type 'list) (print-list (cadr exp) env)]
      [(equal? exp-type 'true) 'True]
      [(equal? exp-type 'false) 'False]
      [(equal? exp-type 'none) 'None]
      [(equal? exp-type 'num) (cadr exp)]
      [else (deref (apply-env (cadr exp) env))])))
  
;prints a list 
(define (print-list list env) 
  (begin
    (display "[")
    (print-items list env)
    (display "\n")))

;creates items of list
(define (print-items list env)
  (if (= (length list) 1)
      (begin
        (display (get-content (car list) env))
        (display "]"))
      (begin
        (display (get-content (car list) env))
        (display ", ")
        (print-items (cdr list) env))))
      

;value of if expression
(define (if-exp exp env)
  (let ((e1 (value-of (cadr exp) env)))
    (cond
      [(or (equal? (cadr e1) 'False) (equal? (cadr e1) null)) (value-of (cadddr exp) (car e1))]
      [(equal? (cadr e1) 'True) (value-of (caddr exp) (car e1))])))


;test
;(define a (car (parse "if True: return 2; else: return 0;;")))
;(value-of a (empty-env))

;for expression
(define (for-exp exp env)
  (let ((var (cadr exp))
        (py-list (cadr
                  (cadr
                   (value-of (caddr exp) env))))
        (statements (cadddr exp)))
    (parsed-for-exp var py-list statements env)))

;for expression after extracting datat from abstract syntax
(define (parsed-for-exp var py-list statements env)
  (if (null? py-list)
      (list env 'None)
      (begin
        (new-ref (cadr (value-of (car py-list) env)))
        (value-of statements (extend-env
                              var
                              (- (length the-store) 1)
                              env))
        (parsed-for-exp var (cdr py-list) statements env))))

;def expression for defining a function
(define (def-exp exp env)
  (let ((pname (cadr exp))
        (vars (caddr exp))
        (stmts (cadddr exp)))
    (list (extend-env-rec pname vars stmts env) 'None)))

;gives the value of a variable
(define (var exp env)
  (let ((val (deref (apply-env (cadr exp) env))))
    (cond
      [(number? val) (list 'num val)]
      [(list? val) (list 'list val)]
      [(equal? #t val) (list 'true)]
      [(equal? #f val) (list 'false)]
      [else (list 'None)])))
      
(define a "a=2;")
(value-of-program a)