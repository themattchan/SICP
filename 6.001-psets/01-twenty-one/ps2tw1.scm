;;; Scheme code for Twenty-One Simulator [PS2 Fall '90]

(define (twenty-one player-strategy house-strategy)
  (let ((house-initial-hand (make-new-hand (deal))))
    (let ((player-hand
           (play-hand player-strategy
                      (make-new-hand (deal))
                      (hand-up-card house-initial-hand))))
      (if (> (hand-total player-hand) 21)
          0                                ; ``bust'': player loses
          (let ((house-hand
                 (play-hand house-strategy
                            house-initial-hand
                            (hand-up-card player-hand))))
            (cond ((> (hand-total house-hand) 21)
                   1)                      ; ``bust'': house loses
                  ((> (hand-total player-hand)
                      (hand-total house-hand))
                   1)                      ; house loses
                  (else 0)))))))           ; player loses

(define (play-hand strategy my-hand opponent-up-card)
  (cond ((> (hand-total my-hand) 21) my-hand) ; I lose... give up
        ((strategy my-hand opponent-up-card) ; hit?
         (play-hand strategy
                    (hand-add-card my-hand (deal))
                    opponent-up-card))
        (else my-hand)))                ; stay

(define (stop-at n)
  (lambda (my-hand opponent-up-card)
    ;; true -> hit | false -> stay
    (< (hand-total my-hand) n)))

(define (test-strategy strat1 strat2 num-tests)
  (define (one-trial)
    ;; 0 -> house win | 1 -> player win
    (twenty-one strat1 strat2))
  (letrec ((loop (lambda (n)
                   (if (= n 0) 0
                       (+ (one-trial) (loop (- n 1)))))))
    (loop num-tests)))

(define (watch-player strat)
  (lambda (my-hand opponent-up-card)
    (define hit? (strat my-hand opponent-up-card))
    (princ "My hand ") (princ my-hand)
    (princ "Opponent up card ") (princ opponent-up-card)
    (princ "Strategy returned: ") (princ (if hit? 'hit 'stay))
    hit?))

(define (lewis my-hand opponent-up-card)
  (cond
   ((< my-hand 12) #t)
   ((> my-hand 16) #f)
   ((= my-hand 12) (< opponent-up-card 4))
   ((= my-hand 16) (not (= opponent-up-card 10)))
   (#t (> opponent-up-card 6))))

(define (both strat1 strat2)
  (lambda (my-hand opponent-up-card)
    (and (strat1 my-hand opponent-up-card)
         (strat2 my-hand opponent-up-card))))

(define (deal) (+ 1 (random 10)))

(define (make-new-hand first-card)
  (make-hand first-card first-card))

(define (make-hand up-card total)
  (cons up-card total))

(define (hand-up-card hand)
  (car hand))

(define (hand-total hand)
  (cdr hand))

(define (hand-add-card hand new-card)
  (make-hand (hand-up-card hand)
             (+ new-card (hand-total hand))))

(define (hit? your-hand opponent-up-card)
  (newline)
  (princ "Opponent up card ")
  (princ opponent-up-card)
  (newline)
  (princ "Your Total: ")
  (princ (hand-total your-hand))
  (newline)
  (princ "Hit? ")
  (user-says-y?))


(define (user-says-y?) (eq? (read-from-keyboard) 'y))

(twenty-one hit? hit?)

(print (test-strategy (stop-at 16) (stop-at 15) 10))
(test-strategy (watch-player (stop-at 16)) (watch-player (stop-at 15)) 2)
(test-strategy lewis (stop-at 15) 10)
(test-strategy lewis (stop-at 16) 10)
(test-strategy lewis (stop-at 17) 10)
