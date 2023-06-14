#lang at-exp racket

(provide (struct-out problem-pair))

;; represents a pair including the name of the problem and the
;; text of the problem.
(struct problem-pair (name text) #:prefab)