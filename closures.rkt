#lang racket

;; lambda over let over lambda - anon counter class
(define (tick)
  (let ((counter 0))
    (values
     (lambda () (set! counter (+ counter 1)))
     (lambda () (set! counter (- counter 1)))
     (lambda () counter))))

;; object 1
(let-values (((inc dec print) (tick)))
  (inc) ;1
  (inc) ;2
  (inc) ;3
  (inc) ;4
  (dec) ;3
  (print))

;; object 2
(let-values (((inc dec print) (tick)))
  (inc)
  (print))