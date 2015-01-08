#lang racket
(require test-engine/racket-tests)

(struct queue (head tail))

(define (make-queue)
  (queue '() '()))

(define queue-empty?
  (lambda (q)
    (and (null? (queue-head q))
         (null? (queue-tail q)))))

(define queue-length
  (lambda (q)
    (+ (length (queue-head q))
       (length (queue-tail q)))))

(define enqueue
  (lambda (q v)
    (queue (queue-head q)
           (cons v (queue-tail q)))))

(define enqueue-list
  (lambda (q vs)
    (queue (queue-head q)
           (append (reverse vs) (queue-tail q)))))

(define dequeue
  (lambda (q)
    (define flip
      (lambda (q)
        (if (null? (queue-head q))
            (queue (reverse (queue-tail q)) '())
            q)))
    (let ((qq (flip q)))
      (cons (car (queue-head qq))
            (queue (cdr (queue-head qq)) (queue-tail qq))))))

(define dequeue-n
  (lambda (q n)
    (define (dequeue-n-iter q n acc)
      (if (or (zero? n)
              (null? q))
          (cons (reverse acc) q)   ; return extracted values in order
          (let ((deq1 (dequeue q)))
            (dequeue-n-iter 
             (cdr deq1)
             (sub1 n)
             (cons (car deq1) acc)))))
    (dequeue-n-iter q n '())))
      
(define queue-filter
  (lambda (q fx)
    (queue (filter fx (queue-head q))
           (filter fx (queue-tail q)))))

(define queue-append
  (lambda (q1 q2)
    (queue (append (queue-head q1)
                   (reverse (queue-tail q1))
                   (queue-head q2))
           (queue-tail q2))))
           
(define list->queue
  (lambda (lst)
    (queue lst '())))

(define queue->list
  (lambda (q)
    (append (queue-head q) (reverse (queue-tail q)))))

;; unit tests
(check-expect 
 (queue->list (cdr (dequeue (list->queue '(1 2 3 4)))))
 '(2 3 4))

(check-expect
 (queue->list (cdr (dequeue (enqueue (list->queue '(1 2 3 4)) 0))))
  '(2 3 4 0))

(check-expect
 (queue->list (queue-filter (enqueue-list (list->queue '(1 2 3 4)) '(5 6 7 8))
               even?))
 '(2 4 6 8))
  
(check-expect
 (queue->list (queue-append (list->queue '(1 2 3 4))
                            (list->queue '(5 6 7 8))))
 '(1 2 3 4 5 6 7 8))
  
(test)