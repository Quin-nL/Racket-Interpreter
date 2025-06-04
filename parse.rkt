#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CSCI 301, Spring 2025
;;
;; Lab #8
;;
;; Quinn Lacrampe
;;
;; Grammar:
;; E -> DN | AS | (L)
;; L -> _L | EL | e
;; S -> AS | e
;; N -> DN | e
;; D -> 0..9     Dgt
;; A -> a..      Sym
;;
;; Predict
;;       Dgt  Sym  (   )    _    $
;; L ->  EL   EL   EL  e    _L   e
;; E ->  DN   AS   (L) err  err  err
;; S ->  e    AS   e   e    e    e
;; N ->  DN   e    e   e    e    e
;; D ->  Dgt  err  err err  err  err
;; A ->  err  Sym  err err  err  err
;;
;; Each function takes a list of the unprocessed input characters and returns
;; a list with the value parsed as the first element followed by the un-parsed
;; input.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide parse E L N D A S)
(require racket/trace)

;; Method for composing syntax error messages
;;
(define syntax-error-msg
  (lambda (msg input)
    (string-append "Syntax Error ("
                   msg
                   "): " (list->string input))
    )
  )

;; Char-Numeric
;;
;; Note that the char-numeric? function is builtin


;; Main parsing function
(define parse
  (lambda (str)
    (first (L (string->list str)))
    )
  )

;; L
;;
;;       Dgt  Sym  (   )    _    $
;; L ->  EL   EL   EL  e    _L   e
(define L
  (lambda (input) 
    (if (null? input) 
        (list '() '()) ; Return empty list of expressions and empty remaining input
        (let ((char (car input)))
          (cond
            ;; L -> _L (whitespace)
            ((char-whitespace? char) 
             (L (cdr input)))
            ;; L -> EL 
            ((or (char-numeric? char) (char-symbolic? char) (eq? char #\())
             (let* ((resE (E input)))  
               (let* ((resL-tail (L (second resE)))) 
                 (list (cons (first resE) (first resL-tail))
                       (second resL-tail)))))
            ;; L -> e 
            (else
             (list '() input)))))))

;; E
;; 
;;       Dgt  Sym  (   )    _    $
;; E ->  DN   AS   (L) err  err  err
(define E
  (lambda (input)
        (let ((char (car input))) ;This variable is useful for the cond dispatch
          (cond
            ;; E -> DN 
            ((char-numeric? char)
             (let ((resD (D input))) 
               (N (second resD) (first resD))))         
            ;; E -> AS
            ((char-symbolic? char)
             (let ((resA (A input))) 
               (S (second resA) (list (first resA)))))
            ;; E -> (L)
            ((eq? char #\()
             (let ((resL (L (cdr input))))
               (if (and (not (null? (second resL))) (eq? (car (second resL)) #\)))
                   (list (first resL) (cdr (second resL))) 
                   (error (syntax-error-msg "Expected ')' after list expression" (second resL))))))
            ))))
;; N
;;
;;       Dgt  Sym  (   )    _    $
;; N ->  DN   e    e   e    e    e
(define N
  (lambda (input inhert)
    (if (null? input)
        (list inhert '()) ; e 
        (let ((char (car input)))
          (if (char-numeric? char) 
              (let* ((digit-val (char->number char)) ; D 
                     (new-acc (+ (* inhert 10) digit-val))) 
                (N (cdr input) new-acc)) 
              (list inhert input)))))) 


;; D
;;
;;       Dgt  Sym  (   )    _    $
;; D ->  Dgt  err  err err  err  err
(define D
  (lambda (input)
    (if (and (not (null? input)) (char-numeric? (car input)))
        (list (char->number (car input)) ;
              (cdr input))
        (error (syntax-error-msg "Expected a digit" input)))))
    
;; S
;;
;;       Dgt  Sym  (   )    _    $
;; S ->  e    AS   e   e    e    e
(define S
  (lambda (input sym) 
    (if (null? input)
        (list (string->symbol (list->string (reverse sym))) '()) ; e
          (if (char-symbolic? (car input)) 
              (S (cdr input) (cons (car input) sym)) ; Recur
              (list (string->symbol (list->string (reverse sym))) input))))) ;e


;; A
;;
;;       Dgt  Sym  (   )    _    $
;; A ->  err  Sym  err err  err  err
(define A
  (lambda (input)
    (if (and (not (null? input)) (char-symbolic? (car input)))
        (list (car input) 
              (cdr input))
        (error (syntax-error-msg "Expected a symbolic character" input)))))

(define char->number
  (lambda (char)
    (- (char->integer char)
       (char->integer #\0))))
;;If char not everything else it is a symbol
(define char-symbolic? 
  (lambda (char)
    (and (not (char-whitespace? char))
         (not (char-numeric? char))  
         (not (eq? char #\())
         (not (eq? char #\))))))

;(trace parse)
;(trace E)
;(trace L)
;(trace N)
;(trace D)
;(trace A)
;(trace S)
