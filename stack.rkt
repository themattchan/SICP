#lang racket
;;; EOPL exercise 2.4
;; list representation of stacks
;; constructors
;; empty-stack : stk
(define empty-stack
  '())
;; push : val stk -> stk
(define push
  (lambda (val stk)
    (cons val stk)))
;; pop : stk -> stk
(define pop
  (lambda (stk)
    (if (empty-stack? stk)
        empty-stack
        (cdr stk))))
;; observers
;; empty-stack? : stk -> boolean
(define empty-stack?
  (lambda (stk)
    (null? stk)))
;; top : stk -> a
(define top
  (lambda (stk)
    (if (empty-stack? stk)
        empty-stack
        (car stk))))

;; exercise 2.12
;; procedural representation of stacks
;; stack = boolean -> val | stk
;; constructors
;; empty-stack : () -> stk
(define empty-stack
  (lambda ()
    (lambda (p)
      'empty)))
;; push : val stk -> stk
(define push
  (lambda (val stk)
    (lambda (p)
      (if p
          val
          stk))))
;; pop : stk -> stk
(define pop
  (lambda (stk)
    (if (empty-stack? stk)
        empty-stack
        (stk false))))
;; observers
;; empty-stack? : stk -> boolean
(define empty-stack?
  (lambda (stk)
    (eq? 'empty (top stk))))
;; top : stk -> val
(define top
 (lambda (stk)
   (if (empty-stack? stk)
        empty-stack
        (stk true))))