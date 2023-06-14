#lang racket

(require (for-syntax syntax/parse))

(provide define-q
         define-qs
         problem-wrap
         (struct-out problem-pair)
         all-question-names
         all-question-texts)


;; define-q associates the name of the question with the problem text,
;; to make it possible to figure out which questions have been used.
;; the lambda wrapper is clunky, but there's a bunch of existing
;; quizzes that treat problems as though they're functions.

(define all-question-names (set))
(define (add-name! name)
  (set! all-question-names (set-add all-question-names name)))

;; a reversed list of all defined problems
(define all-question-texts (list))
(define (add-text! problem)
  (set! all-question-texts (cons problem all-question-texts)))

(define-syntax (define-q stx)
  (syntax-parse stx
    [(_ (tags:id ...) name:id expr)
     #'(begin
         (add-name! (quote name))
         (define name
           (let ([this-problem
                  (λ (points)
                    (let ([problem-proc expr])
                      (unless (procedure? problem-proc)
                        (raise-argument-error 'problem-maker
                                              "procedure"
                                              0 problem-proc (quote name)))
                      (problem-pair/chk (quote name)
                                        (problem-proc points)
                                        (quote (tags ...)))))])
             (add-text! (list 'problem this-problem))
             this-problem)))]
    [(_ () (name:id args:id ...) expr)
     #'(begin
         (add-name! (quote name))
         (define (name args ...)
           (let ([this-problem
                  (λ (points)
                    (let ([problem-proc expr])
                      (unless (procedure? problem-proc)
                        (raise-argument-error 'problem-maker
                                              "procedure"
                                              0 problem-proc (quote name)))
                      (problem-pair/chk (quote name)
                                        (problem-proc points))))])
             (add-text! (list 'problem-producer this-problem))
             this-problem)))]))

(define-syntax (define-qs stx)
  ;; not sure of the nicest way to abstract over these...
  (syntax-parse stx
    [(_ () name:id expr)
     #'(define name
         (let ([probs expr])
           (for/list ([str (in-list probs)]
                      [i (in-naturals)])
             (define indexed-name
               (string->symbol (~a (quote name) "-" (add1 i))))
             (add-name! indexed-name)
             (let ([this-problem (λ (points)
                              (problem-pair/chk indexed-name
                                                (str points)))])
               #;(add-text! (list 'problem this-problem))
               this-problem))))]))

;; for use in situations where the name can't be extracted from the 'define'
(define (problem-wrap name p)
  (unless (string? name)
    (raise-argument-error 'problem-wrap "string" 0 name))
  (unless (procedure? p)
    (raise-argument-error 'problem-wrap "problem" 1 p))
  (add-name! (string->symbol name))
  (let ([problem (λ (points)
                   (problem-pair/chk (string->symbol name) (p points)))])
    (add-text! problem)
    problem))

(define (problem-pair/chk name text [tag '()])
  (when (not (symbol? name))
    (raise-argument-error 'problem-pair/chk
                          "symbol"
                          0 name text tag))
  (when (not (or (symbol? tag) (and (list? tag) (andmap symbol? tag))))
    (raise-argument-error 'problem-pair/chk
                          "symbol or list of symbols"
                          2 name text tag))
  (define tags (cond [(symbol? tag) (list tag)]
                     [else tag]))
  (problem-pair name text tags))

;; represents a pair including the name of the problem and the
;; text of the problem.
(struct problem-pair (name text tags) #:prefab)