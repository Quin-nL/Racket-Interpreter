#lang racket
;;Quinn Lacampe
;;5/15/2025
;;From Scott Wehrwein; CSCI 301 Lab 6
;;e1 environment and add
(define add
  (lambda (a b)
    (cond ((number? a) (+ a b))
          ((list? a) (append a b))
          (else (error "unable to add" a b)))))
(define e1  (map list
                 '(     x  y  z ls         + - * cons car cdr nil list add = nil? else)
                 (list 5 8 10 (list 1 2) + - * cons car cdr '() list add = empty? #t)))

(define closure (lambda (vars body env) (list 'closure vars body env)))
(define closure? (lambda (clos) (and (pair? clos) (eq? (car clos) 'closure))))
(define closure-vars cadr)
(define closure-body caddr)
(define closure-env cadddr)

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
;;Evaluate conditional statements.
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
      [else error "special form not recognized"]
  ))
;;Function to apply closure
(define (apply-closure close-item closure)
  (let* ([bindings (cadr close-item)] 
         [body (caddr close-item)])   
    ;;Create the list of new environment additions, e.g., '((var1 val1) (var2 val2))
    (define new-env-additions (map list (map car bindings) (map (lambda (binding-pair)
                        (evaluate (cadr binding-pair) closure));;maybe this is not necessary edge case handling.
                      bindings)))
    (evaluate body (append new-env-additions closure))
    ))
;;Function to apply lambda
(define (apply-function expr env)
  print expr
  (cond
    [(procedure?)(evaluate (car expr)env) 
         (apply(evaluate (car expr) env)(map (lambda (john)  (evaluate john env)) (cdr expr)))]
    [(closure?) (apply-closure expr closure)]
    [ (error "apply-function error")])
)
;;Function to check if a list starts with a special form
(define (special-form? input)
  (if (or (equal? (car input) 'if)
          (equal? (car input) 'cond)
          (equal? (car input) 'let)
          (equal? (car input) 'lambda))
        #t 
        #f))
;;function to lookup symbols
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
    [(list? expr) (apply-function (car expr) env)]
    [(error "evaluate error")])
  )
;;provide functions for tests
(provide evaluate lookup special-form? evaluate-special-form)

