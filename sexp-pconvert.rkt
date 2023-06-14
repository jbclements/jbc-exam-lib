#lang typed/racket

(provide my-pconvert
         my-pconvert/sexp)

(require typed/rackunit)

(define (my-pconvert [sexp : Sexp]) : String
  (match sexp
    ['() "'()"]
    [(cons a b) (~a "(cons "(my-pconvert a)" "(my-pconvert b)")")]
    [(? symbol? s) (~a "'"s)]
    [(? number? n) (~a n)]))

(check-equal? (my-pconvert '(4 x ((y))))
              "(cons 4 (cons 'x (cons (cons (cons 'y '()) '()) '())))")




(define (my-pconvert/sexp [sexp : Sexp]) : Sexp
  (match sexp
    ['() (list 'quote '())]
    [(cons a b) (list 'cons (my-pconvert/sexp a) (my-pconvert/sexp b))]
    [(? symbol? s) (list 'quote s)]    
    [(? number? n) n]))

(check-equal? (my-pconvert/sexp '(4 x ((y))))
              '(cons 4 (cons 'x (cons (cons (cons 'y '()) '()) '()))))