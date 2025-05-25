#lang racket
;;Quinn Lacampe
;;5/20/2025
;;From Scott Wehrwein; CSCI 301 Lab 7
;;e1 environment and add
(define add
  (lambda (a b)
    (cond ((and (number? a) (number? b)) (+ a b))
          ((and (list? a) (list? b)) (append a b))
          (else (error "unable to add" a b)))))

(define e1 (map list
                 '(x y z + - * cons car cdr nil empty? = equal? < else  add list)
             (list 2 4 6 + - * cons car cdr '() empty? = equal? < #t    add list)))

(define closure
(lambda (vars body env)
(mcons 'closure (mcons env (mcons vars body)))))
(define closure?
(lambda (clos) (and (mpair? clos) (eq? (mcar clos) 'closure))))
(define closure-env
(lambda (clos) (mcar (mcdr clos))))
(define closure-vars
(lambda (clos) (mcar (mcdr (mcdr clos)))))
(define closure-body
(lambda (clos) (mcdr (mcdr (mcdr clos)))))
(define set-closure-env!
(lambda (clos new-env) (set-mcar! (mcdr clos) new-env)))


;;Offers support for cond and its nested statements
(define (cond-helper C-item env)
  (cond
    [(equal? (caar C-item) 'else) (evaluate (cadar C-item) env)]
    [else (if (equal? (evaluate (caar C-item) env) #t)
              (evaluate (cadar C-item) env)
              (cond-helper (cdr C-item) env))
          ]))
;;Function to support let
(define (let-helper let-item env)
  (let* ([bindings (cadr let-item)] 
         [body (caddr let-item)])   
    ;;Create the list of new environment additions, e.g., '((var1 val1) (var2 val2))
    (define new-env-additions (map list (map car bindings) (map (lambda (binding-pair)
                        (evaluate (cadr binding-pair) env))
                      bindings)))
    (evaluate body (append new-env-additions env))
    ))

;;Function to apply closure
(define (apply-closure close-item close-vals)
  (let* ([bindings (closure-vars close-item)] 
         [body(closure-body close-item)]
         [saved-env(closure-env close-item)])   
    ;;Create the list of new environment additions, e.g., '((var1 val1) (var2 val2))
    (define new-env-additions (map list bindings close-vals))
    (evaluate body (append new-env-additions saved-env))
    ))
;;Function to apply lambda
(define (apply-function expr env) 
  (cond
    [(procedure? expr)(apply expr env)]
    [(closure? expr) (apply-closure expr env)]
    [(error "apply-function error")])
)
;;Function to support a recursive let
(define (let-rec expr old-env)
  (let* ([bindings (cadr expr)]  
         [body (caddr expr)]
         [vars (map car bindings)]
         [val-exprs (map cadr bindings)])

   
    (define mini-vals (map (lambda (v-expr) (evaluate v-expr old-env)) val-exprs))
    (define mini-env (map list vars mini-vals))
    (define new-env (append mini-env old-env))
    (for-each
     (lambda (pair)
       (let ([val (cadr pair)]) 
         (when (closure? val)
           (set-closure-env! val new-env))))
     mini-env)
    (evaluate body new-env)))
;;Evaluate conditional statements, or call the helper-functions
(define (evaluate-special-form item env) 
  (cond 
    [(equal? (car item) 'if) 
        (if (equal? (evaluate (cadr item) env) #t) 
          (evaluate (caddr item) env)
          (evaluate (cadddr item) env))]
      [(equal? (car item) 'cond)
       (if (equal? (evaluate (caadr item) env) #t)
           (evaluate (cadadr item) env)
           (cond-helper (cddr item) env))]
      [(equal? (car item) 'let)(let-helper item env)]
      [(equal? (car item) 'lambda)(closure (cadr item)(caddr item) env)]
      [(equal? (car item) 'letrec)(let-rec item env)]
      [else error "special form not recognized"]
  ))
;;Function to check if a list starts with a special form
(define (special-form? input)
  (if (or (equal? (car input) 'if)
          (equal? (car input) 'cond)
          (equal? (car input) 'let)
          (equal? (car input) 'lambda)
          (equal? (car input) 'letrec))
        #t 
        #f))
;;Function to lookup symbols
(define(lookup sym env)
(cond 
  [(equal? (length env) 0)(error "No envirorment provided")] 
  [(not(symbol? sym)) (error "Not a Symbol" sym)]
  [(equal? (caar env) sym)(cadar env) ] 
  [else (lookup sym (cdr env))])
)
;Function to evaluate input
(define(evaluate expr env)
  (cond
    [(number? expr)expr]
    [(symbol? expr)(lookup expr env)]
    [(special-form? expr)(evaluate-special-form expr env)]
    [(list? expr) 
     (apply-function (evaluate (car expr) env)
                 (map (lambda (arg) (evaluate arg env)) (cdr expr)))]
    [(error "evaluate error")])
  )
;;provide functions for tests
(provide evaluate lookup special-form? evaluate-special-form)

