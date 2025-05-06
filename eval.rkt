#lang racket
;;Quinn Lacampe
;;5/5/2025
;;Lab 5
;;e1 environment and add
(define add
  (lambda (a b)
    (cond ((number? a) (+ a b))
          ((list? a) (append a b))
          (else (error "unable to add" a b)))))
(define e1  (map list
                 '(     x  y  z ls         + - * cons car cdr nil list add = nil? else)
                 (list 10 20 30 (list 1 2) + - * cons car cdr '() list add = empty? #t)))
;;function to lookup symbols
(define(lookup sym env)
(cond 
  [(equal? (length env) 0)(error "No envirorment provided")] 
  [(not(symbol? sym)) (error "Not a Symbol" sym)]
  [(equal? (caar env) sym)(cadar env) ] 
  [else (lookup sym (cdr env))])
)
;;Offers support for cond and its nested statements
(define (cond-helper C-item env)
  (cond
    [(equal? (caar C-item) 'else) (evaluate (cadar C-item) env)]
    [else (if (equal? (evaluate (caar C-item) env) #t)
              (evaluate (cadar C-item) env)
              (cond-helper (cdr C-item) env))
          ]))
;;Function to support let
(define (let-helper L-item env)
(let ([new-pair
       (cons (caaadr L-item)
             (cons (car (cdaadr L-item)) '()))]) ; construct new variable
(if (procedure? (car L-item)) ;Check for expression
    (cons new-pair env)
    (cons new-pair (let-helper L-item env)))
 (evaluate (L-item) env);combine new-pair, recur 
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
      [else error "special form not recognized"]
  ))
;;Function to check if a list starts with a special form
(define (special-form? input)
  (if (or (equal? (car input) 'if)
          (equal? (car input) 'cond)
          (equal? (car input) 'let))
        #t 
        #f))
;Function to evaluate input
(define(evaluate expr env)
  (cond
    [(number? expr)expr]
    [(symbol? expr)(lookup expr env)]
    [(special-form? expr)(evaluate-special-form expr env)]
    [(list? expr) 
     (if (procedure? (evaluate (car expr)env))
         (apply(evaluate (car expr) env)(map (lambda (john)  (evaluate john env)) (cdr expr)))
         (error "First item in a list must be a procedure"(car expr)))])
  )
;;provide functions for tests
(provide evaluate lookup special-form? evaluate-special-form)

