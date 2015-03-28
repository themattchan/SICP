;;;;CODE FROM CHAPTER 2 OF STRUCTURE AND INTERPRETATION OF COMPUTER PROGRAMS

;;; Examples from the book are commented out with ;: so that they
;;;  are easy to find and so that they will be omitted if you evaluate a
;;;  chunk of the file (programs with intervening examples) in Scheme.

;;; BEWARE: Although the whole file can be loaded into Scheme,
;;;  you won't want to do so.  For example, you generally do
;;;  not want to use the procedural representation of pairs
;;;  (cons, car, cdr as defined in section 2.1.3) instead of
;;;  Scheme's primitive pairs.

;;; Some things require code from other chapters -- see ch2support.scm


(define (linear-combination a b x y)
  (+ (* a x) (* b y)))

(define (linear-combination a b x y)
  (add (mul a x) (mul b y)))


;;;SECTION 2.1.1

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

;: (define x (cons 1 2))
;:
;: (car x)
;: (cdr x)

;: (define x (cons 1 2))
;: (define y (cons 3 4))
;: (define z (cons x y))
;: (car (car z))
;: (car (cdr z))

(define (make-rat n d) (cons n d))

(define (numer x) (car x))

(define (denom x) (cdr x))

;;footnote -- alternative definitions
(define make-rat cons)
(define numer car)
(define denom cdr)

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))


;: (define one-half (make-rat 1 2))
;:
;: (print-rat one-half)
;:
;: (define one-third (make-rat 1 3))
;:
;: (print-rat (add-rat one-half one-third))
;: (print-rat (mul-rat one-half one-third))
;: (print-rat (add-rat one-third one-third))


;; reducing to lowest terms in constructor
;; (uses gcd from 1.2.5 -- see ch2support.scm)

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))


;: (print-rat (add-rat one-third one-third))

;; EXERCISE 2.1
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (< d 0)
        (cons (/ (* -1 n) g) (/ (* -1 d) g))
        (cons (/ n g) (/ d g)))))

;;;SECTION 2.1.2

;; reducing to lowest terms in selectors
;; (uses gcd from 1.2.5 -- see ch2support.scm)

(define (make-rat n d)
  (cons n d))

(define (numer x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (car x) g)))

(define (denom x)
  (let ((g (gcd (car x) (cdr x))))
    (/ (cdr x) g)))


;; EXERCISE 2.2
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; points
;; make-point : Num Num -> Point
(define (make-point x y) (cons x y))
(define (x-point pt) (car pt))
(define (y-point pt) (cdr pt))

;; line segments
;; make-segment : Point Point -> Line
(define (make-segment start end) (cons start end))
(define (start-segment line) (car line))
(define (end-segment line) (cdr line))

(define (midpoint-segment line)
  (let ((mx (/ (+ (x-point (start-segment line))
                  (x-point (end-segment line))) 2))
        (my (/ (+ (y-point (start-segment line))
                  (y-point (end-segment line))) 2)))
  (make-point mx my)))

;; EXERCISE 2.3
;; rectangle 1: top left and bottom right corner
;; make-rect : Point Point -> Rect
(define (make-rect tl br) (cons tl br))
(define (rect-width r) (abs (- (x-point (car r)) (x-point (cdr r)))))
(define (rect-height r) (abs (- (y-point (car r)) (y-point (cdr r)))))

(define (rect-perimeter r)
  (* 2
     (+ (rect-width r) (rect-height r))))

(define (rect-area r)
  (* (rect-width r) (rect-height r)))

;; rectangle 2: top left corner, width and height
(define (make-rect tl w h) (cons tl (cons w h)))
(define (rect-width r) (car (cdr r)))
(define (rect-height r) (cdr (cdr r)))

;;;SECTION 2.1.3
(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1 -- CONS" m))))
  dispatch)

(define (car z) (z 0))

(define (cdr z) (z 1))


;; EXERCISE 2.4
#|
Evaluation:
cons: x y -> m -> 'a where a = x or y
car: call m. m expects a function of two inputs that will return a value.
|#
(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

;; EXERCISE 2.5
;; cons, car and cdr for nonnegative integers
(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

#|
count the number of occurrences of d in n
cons -> (2 2 2 2... a times) * (3 3 3... b times), these products are
separable, one is always even and the other is always odd.
|#
(define (count-occs n d)
  (define (go c acc)
    (if (= 0 (remainder c d))
        (go (/ c d) (+ 1 acc))
        acc))
  (go n 0))

(define (car z)
  (count-occs z 2))
(define (cdr z)
  (count-occs z 3))

;; EXERCISE 2.6
(define zero
  (lambda (f)
    (lambda (x)
      x)))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

#| Church numerals
zero : \f.\x.x
inc  : \f.\x.(f ((n f) x))

What happens if we apply add one to zero?

\f.\x. (f ((\f.\x.x).f).x)
=> \f.\x. (f (\x.x).x)
=> \f.\x. (f x)

and if we add one to that?
\f.\x.(f (((\f.\x. (f x)).f) x))
=> \f.\x.(f (\x. (f x) x))
=> \f.\x.(f (f x))

... etc
|#

(define one
  (lambda (f)
    (lambda (x)
      (f x))))

(define two
  (lambda (f)
    (lambda (x)
      (f (f x)))))

(define (add-church m n)
  (lambda (f)
    (lambda (x)
      ((m f) ((n f) x)))))


;;;SECTION 2.1.4

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (define (spans-zero?)
    (if (and (<= (lower-bound y) 0)
             (>= (upper-bound y) 0))))
  (if (spans-zero?)
      (error "Error: denom spans 0")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

;; EXERCISE 2.7

(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

;; EXERCISE 2.8
;; interval addition -> [a,b] + [c,d] = [a+c, b+d]
;; interval subtraction -> [a,b] + [c,d] = [a-d, b-c]
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

;; EXERCISE 2.9
#|
We show that the width of the sum is a function only of the width of the
intervals being added.

Direct proof.

let a sum of intervals be [a,b] + [c,d] = [a+c, b+d], its width therefore is
((b+d) - (a+c))/2

the width of each interval:
width([a,b]) = (b-a)/2
width([c,d]) = (d-c)/2

summing, we get (b-a+d-c)/2 => ((b+d) - (a+c))/2

A similar argument can be made for differences. Therefore, the width of the
sum/diff of two intervals is a function of only the widths being added. QED.

Let us now consider the case with multiplication or division.

In multiplication/division, because the min/max functions are introduced, the
width function is no longer distributive.

For example, if ac is the min and bd the max, then the width is (bd-ac)/2
But the widths of the constituent intervals are
width([a,b]) = (b-a)/2
width([c,d]) = (d-c)/2

Multiplying, we have (bd-bc-ad+ac)/4

We have reached a contradiction; the width function is not distributive and
hence it is not a function only of the widths of the intervals considered.
|#

(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

(define a (make-interval 10 20))
(define b (make-interval 2 8))
(* (width a) (width b))                 ; 15
(width (mul-interval a b))              ; 70

;;;SECTION 2.1.4 again

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; EXERCISE 2.12
;; percentage is a number from 0.00 to 100.00
(define (make-center-percent c p)
  (make-center-width c (* c (/ p 100.00))))

;; EXERCISE 2.13
(define (percent i)
  (* 100.00 (/ (width i) (center i))))

;; parallel resistors

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))
;; EXERCISE 2.14...

;;;SECTION 2.2.1

;: (cons 1
;:       (cons 2
;:             (cons 3
;:                   (cons 4 nil))))


;: (define one-through-four (list 1 2 3 4))
;:
;: one-through-four
;: (car one-through-four)
;: (cdr one-through-four)
;: (car (cdr one-through-four))
;: (cons 10 one-through-four)

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

;: (define squares (list 1 4 9 16 25))

;: (list-ref squares 3)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

;: (define odds (list 1 3 5 7))

;: (length odds)

(define (length items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

;: (append squares odds)
;: (append odds squares)


(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))


;; EXERCISE 2.17
(define (last-pair lst)
  (if (null? (cdr lst))
      lst
      (last-pair (cdr lst))))

;: (last-pair (list 23 72 149 34))

;; EXERCISE 2.18
(define (reverse lst)
  (if (null? lst)
      '()
      (append (reverse (cdr lst)) (list (car lst)))))

 (reverse (list 1 4 9 16 25))

;; EXERCISE 2.19
(define us-coins (list 50 25 10 5 1))

(define uk-coins (list 100 50 20 10 5 2 1 0.5))

;; return the first coin value
(define (first-denomination coin-values)
  (car coin-values))

;; return rest of coin values
(define (except-first-denomination coin-values)
  (cdr coin-values))

;; if list is empty
(define (no-more? coin-values)
  (null? coin-values))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

;: (cc 100 us-coins)

#| order doesn't matter, all we're doing would be to rearrange the branchs of
the tree recursion, but they get summed in the end |#

;; EXERCISE 2.20
(define (same-parity fst . rst)
  (define (go things acc)
    (if (null? things)
        acc
        (go (cdr things)
            (if (= (remainder fst 2)
                   (remainder (car things) 2))
                (append acc (list (car things)))
                acc))))
  (go rst (list fst)))

;: (same-parity 1 2 3 4 5 6 7)
;: (same-parity 2 3 4 5 6 7)


;; Mapping over lists

(define (scale-list items factor)
  (if (null? items)
      nil
      (cons (* (car items) factor)
            (scale-list (cdr items) factor))))

;: (scale-list (list 1 2 3 4 5) 10)

;: (map + (list 1 2 3) (list 40 50 60) (list 700 800 900))

;: (map (lambda (x y) (+ x (* 2 y)))
;:      (list 1 2 3)
;:      (list 4 5 6))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

;: (map abs (list -10 2.5 -11.6 17))

;: (map (lambda (x) (* x x))
;:      (list 1 2 3 4))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))


;; EXERCISE 2.21
(define (square-list items)
  (if (null? items)
      '()
      (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map (lambda (x) (* x x)) items))

;: (square-list (list 1 2 3 4))


;; EXERCISE 2.22
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

#| Because in the iterative process, the computed value (car) is being consed to
the accumulator at every iteration |#

;; EXERCISE 2.23
(define (for-each f lst)
  (if (null? lst)
      #t
      (begin
        (f (car lst))
        (for-each f (cdr lst)))))

;: (for-each (lambda (x) (newline) (display x))
;:           (list 57 321 88))



;;;SECTION 2.2.2
;: (cons (list 1 2) (list 3 4))
;:
;: (define x (cons (list 1 2) (list 3 4)))
;: (length x)
;: (count-leaves x)
;:
;: (list x x)
;: (length (list x x))
;: (count-leaves (list x x))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;; EXERCISE 2.24
;: (list 1 (list 2 (list 3 4)))

;; EXERCISE 2.25
;; cdr, cdr, car cdr, car (of course, these will be in reverse were we to write lisp)
;: (1 3 (5 7) 9)
;; car, car
;: ((7))
;; six pairs of cdr, car
;: (1 (2 (3 (4 (5 (6 7))))))

;; (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr
;;   (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))))))))))

;; EXERCISE 2.26
;: (define x (list 1 2 3))
;: (define y (list 4 5 6))
;:
;: (append x y) -> '(1 2 3 4 5 6)
;: (cons x y) -> '((1 2 3) 4 5 6)
;: (list x y) -> '((1 2 3) (4 5 6))

;; EXERCISE 2.27
(define (deep-reverse lst)
  (cond ((null? lst) '())
        ((pair? (car lst))
         (append (deep-reverse (cdr lst))
                 (list (deep-reverse (car lst)))))
         (else
          (append (deep-reverse (cdr lst))
                  (list (car lst))))))

;: (define x (list (list 1 2) (list 3 4)))
;: x
;: (reverse x)
;: (deep-reverse x)


;; EXERCISE 2.28
(define (fringe tree)
  (cond ((null? tree) '())
        ;; not list
        ((not (pair? tree)) (list tree))
        (else
         ;; recursively traverse the list
         (append (fringe (car tree))
                 (fringe (cdr tree))))))

;: (define x (list (list 1 2) (list 3 4)))
;: (fringe x)
;: (fringe (list x x))


;; EXERCISE 2.29
#| A note on the structure:
The data abstractions mobile and branch are mutually recursive, i.e.
a mobile is a pair of branches,
and a branch is an object with a length that either holds a weight (terminal
case) or a mobile.

Visually:
                      Mobile
                      /     \
    (branch l1 Mobile)     (branch l2 w)
               /    \
      (branch l3 w) (branch l4 Mobile)
                                  |
                               ......

Because this structure is mutually recursive, our functions are mutually
recursive as well, and will be defined in pairs.
|#
;; Constructor and accessors for mobile
(define (make-mobile left right)
  (list left right))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

;; Constructor and accessor for branch
(define (make-branch length structure)
  (list length structure))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

(define (branch-weight branch)
  (if (pair? (branch-structure branch))
      (total-weight (branch-structure branch))
      (branch-structure branch)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

;; (define a (make-mobile
;;            (make-branch 2 (make-mobile (make-branch 2 3) (make-branch 2 3)))
;;            (make-branch 2 3)))

;; (total-weight a)

(define (branch-torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define (branch-balanced? branch)
  (if (pair? (branch-structure branch))
      (balanced? (branch-structure branch))
      #t))

(define (balanced? mobile)
  (and (= (branch-torque (left-branch mobile))
          (branch-torque (right-branch mobile)))
       (branch-balanced? (left-branch mobile))
       (branch-balanced? (right-branch mobile))))

;; (balanced? a)


;; part d
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

;; only have to change accessors using cadr:
;; (cdr (list 1 2)) => (2)
;; (cdr (cons 1 2)) => 2


(define (right-branch mobile)
  (cdr mobile))

(define (branch-structure branch)
  (cdr branch))

;; Mapping over trees

(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))


;: (scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))
;:             10)


(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))


;; EXERCISE 2.30
(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))

;; (square-tree
;;  (list 1
;;        (list 2 (list 3 4) 5)
;;        (list 6 7)))

;; EXERCISE 2.31
(define (tree-map fun tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map fun sub-tree)
             (fun sub-tree)))
       tree))

(define (square-tree tree) (tree-map square tree))


;; EXERCISE 2.32
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map
                      (lambda (t) (cons (car s) t))
                      rest)))))

(subsets '(1 2 3))

;;;SECTION 2.2.3

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        nil        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))


;; Sequence operations
;: (map square (list 1 2 3 4 5))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

;: (filter odd? (list 1 2 3 4 5))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;: (accumulate + 0 (list 1 2 3 4 5))
;: (accumulate * 1 (list 1 2 3 4 5))
;: (accumulate cons nil (list 1 2 3 4 5))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

;: (enumerate-interval 2 7)

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

;: (enumerate-tree (list 1 (list 2 (list 3 4)) 5))

(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(define (even-fibs n)
  (accumulate cons
              nil
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))

(define (list-fib-squares n)
  (accumulate cons
              nil
              (map square
                   (map fib
                        (enumerate-interval 0 n)))))

;: (list-fib-squares 10)


(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
              1
              (map square
                   (filter odd? sequence))))

;: (product-of-squares-of-odd-elements (list 1 2 3 4 5))

(define (salary-of-highest-paid-programmer records)
  (accumulate max
              0
              (map salary
                   (filter programmer? records))))

;; EXERCISE 2.33
(define (map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y))
              nil sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate (lambda (x y)
                (+ 1 y))
                0 sequence))

;; EXERCISE 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* this-coeff x) higher-terms))
              0
              coefficient-sequence))

;: (horner-eval 2 (list 1 3 0 5 0 1))

;; EXERCISE 2.35
(define (count-leaves t)
  (accumulate (lambda (x a) (+ 1 a)) 0 (fringe t)))

;; (count-leaves '((1 2) (3 4)))

;; EXERCISE 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

#|
(let ((s '((1 2 3) (4 5 6) (7 8 9) (10 11 12))))
  (accumulate-n + 0 s))
|#

;; EXERCISE 2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product v x)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (rows) (matrix-*-vector cols rows)) m)))


;; EXERCISE 2.38

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;; copy of accumulate
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

#|
;: (fold-right / 1 (list 1 2 3))

(/ 1 (/ 2 (/ 3 1)))
==> 3/2

;: (fold-left / 1 (list 1 2 3))

(iter 1 '(1 2 3))
(iter (/ 1 1) '(2 3))
(iter (/ (/ 1 1) 2) '(3))
(iter (/ (/ (/ 1 1) 2) 3) '())
(/ (/ (/ 1 1) 2) 3)
==> 1/6

;: (fold-right list nil (list 1 2 3))

(list 1 (list 2 (list 3 nil)))
==> '(1 (2 (3 ())))

;: (fold-left list nil (list 1 2 3))

(iter nil '(1 2 3))
(iter (list nil 1) '(2 3))
(iter (list (list nil 1) 2) '(3))
(iter (list (list (list nil 1) 2) 3) '())
(list (list (list nil 1) 2) 3)
==> '(((() 1) 2) 3)

|#

;; EXERCISE 2.39

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))

;;Nested mappings

;: (accumulate append
;:             nil
;:             (map (lambda (i)
;:                    (map (lambda (j) (list i j))
;:                         (enumerate-interval 1 (- i 1))))
;:                  (enumerate-interval 1 n)))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))

;; EXERCISE 2.40
(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(define (permutations s)
  (if (null? s)                         ; empty set?
      (list nil)                        ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

;; EXERCISE 2.41
(define (enumerate-interval bot top)
  (if (> bot top)
      '()
      (cons bot (enumerate-interval (+ 1 bot) top))))

(define (ordered-triples n s)
  (define (sum xs)
    (accumulate + 0 xs))
  (let ((range (enumerate-interval 1 n)))
    (filter (lambda (triple) (= (sum triple) s))
            (flatmap
             (lambda (i)
               (flatmap
                (lambda (j)
                  (map (lambda (k) (list i j k)) range))
                range))
             range)
            )))

;; EXERCISE 2.42
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;; EXERCISE 2.43
;; Louis's version of queens
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
     ;; next expression changed
         (flatmap
      (lambda (new-row)
        (map (lambda (rest-of-queens)
           (adjoin-position new-row k rest-of-queens))
         (queen-cols (- k 1))))
      (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

;;;SECTION 2.2.4

;: (define wave2 (beside wave (flip-vert wave)))
;: (define wave4 (below wave2 wave2))


(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))


;: (define wave4 (flipped-pairs wave))


(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))


(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))


(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))


;; Higher-order operations

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))


(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

; footnote
;: (define flipped-pairs
;:   (square-of-four identity flip-vert identity flip-vert))


(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))


;; EXERCISE 2.45

;: (define right-split (split beside below))
;: (define up-split (split below beside))


;; Frames

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))


;: ((frame-coord-map a-frame) (make-vect 0 0))

;: (origin-frame a-frame)


;; EXERCISE 2.47

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))


;; Painters

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))


(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (sub-vect (m corner1) new-origin)
                     (sub-vect (m corner2) new-origin)))))))


(define (flip-vert painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)    ; new origin
                     (make-vect 1.0 1.0)    ; new end of edge1
                     (make-vect 0.0 0.0)))  ; new end of edge2


(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect 0.5 0.5)
                     (make-vect 1.0 0.5)
                     (make-vect 0.5 1.0)))


(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))


(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))


(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-point
                              (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-point
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

;;;SECTION 2.3.1

;: (a b c d)
;: (23 45 17)
;: ((Norah 12) (Molly 9) (Anna 7) (Lauren 6) (Charlotte 3))

;: (* (+ 23 45) (+ x 9))

(define (fact n) (if (= n 1) 1 (* n (fact (- n 1)))))


;: (define a 1)
;: (define b 2)
;: (list a b)
;: (list 'a 'b)
;: (list 'a b)

;: (car '(a b c))
;: (cdr '(a b c))


(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;: (memq 'apple '(pear banana prune))
;: (memq 'apple '(x (apple sauce) y apple pear))


;; EXERCISE 2.53
;: (list 'a 'b 'c)
;:
;: (list (list 'george))
;:
;: (cdr '((x1 x2) (y1 y2)))
;:
;: (cadr '((x1 x2) (y1 y2)))
;:
;: (pair? (car '(a short list)))
;:
;: (memq 'red '((red shoes) (blue socks)))
;:
;: (memq 'red '(red shoes blue socks))


;; EXERCISE 2.54
;: (equal? '(this is a list) '(this is a list))
;: (equal? '(this is a list) '(this (is a) list))

;; EXERCISE 2.55
;: (car ''abracadabra)


;;;SECTION 2.3.2

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))

;; representing algebraic expressions

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))


;: (deriv '(+ x 3) 'x)
;: (deriv '(* x y) 'x)
;: (deriv '(* (* x y) (+ x 3)) 'x)


;; With simplification

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))


;: (deriv '(+ x 3) 'x)
;: (deriv '(* x y) 'x)
;: (deriv '(* (* x y) (+ x 3)) 'x)


;; EXERCISE 2.57
;: (deriv '(* x y (+ x 3)) 'x)


;;;SECTION 2.3.3

;; UNORDERED

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))
        ))

;; EXERCISE 2.59

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1)
                    (union-set (cdr set1) set2)))))

;; EXERCISE 2.60

;; UNORDERED WITH DUPLICATES

;; element-of-set? is the same

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (append set1 set2))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (intersection-set (cdr set1)
                           (cons (car set1) set2)))
        (else (intersection-set (cdr set1) set2))
        ))

#|
The adjoin and intersection operations are now constant time. Element-of-set? is
still O(n), but because a set with duplicates will be much larger, n will be
larger meaning a slower running time in operation. intersection-set will not
remove duplicates if we use the original implementation, but the tail-recursive
one will remove duplicates from set1.

Use this if the use case involves lots of adding and unions but few queries and
intersections, and if you know the elements you are adding contain few
duplicates.

|#

;; ORDERED

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

;; EXERCISE 2.61
(define (adjoin-set x set)
  (cond ((null? set) '(x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set)
                    (element-of-set? x (cdr set))))))

;; EXERCISE 2.62
(define (union-set set1 set2)
  (cond
   ((null? set1) set2)
   ((null? set2) set1)
   (else
    (let ((x1 (car set1)) (x2 (car set2)))
      (cond
       ((= x1 x2)
        (cons x1 (union-set (cdr set1) (cdr set2))))
       ((< x1 x2)
        (cons x1 (union-set (cdr set1) set2)))
       ((< x2 x1)
        (cons x2 (union-set set1 (cdr set2)))))))))

;; BINARY TREES
(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))


;; EXERCISE 2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))


;; EXERCISE 2.64

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;; INFORMATION RETRIEVAL

(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup given-key (cdr set-of-records)))))


;;;SECTION 2.3.3

;; representing

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

;; decoding
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

;; sets

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))


;; EXERCISE 2.67

;: (define sample-tree
;:   (make-code-tree (make-leaf 'A 4)
;:                   (make-code-tree
;:                    (make-leaf 'B 2)
;:                    (make-code-tree (make-leaf 'D 1)
;:                                    (make-leaf 'C 1)))))

;: (define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))


;; EXERCISE 2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;; EXERCISE 2.69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;;;SECTION 2.4.1

;: (make-from-real-imag (real-part z) (imag-part z))

;: (make-from-mag-ang (magnitude z) (angle z))

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))


;; Ben (rectangular)

(define (real-part z) (car z))

(define (imag-part z) (cdr z))

(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))

(define (angle z)
  (atan (imag-part z) (real-part z)))

(define (make-from-real-imag x y) (cons x y))

(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))


;; Alyssa (polar)

(define (real-part z)
  (* (magnitude z) (cos (angle z))))

(define (imag-part z)
  (* (magnitude z) (sin (angle z))))

(define (magnitude z) (car z))

(define (angle z) (cdr z))

(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))

(define (make-from-mag-ang r a) (cons r a))


;;;SECTION 2.4.2

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))


;; Ben (rectangular)

(define (real-part-rectangular z) (car z))

(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

;; Alyssa (polar)

(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))

(define (magnitude-polar z) (car z))

(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
               (cons (sqrt (+ (square x) (square y)))
                     (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))


;; Generic selectors

(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type -- REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type -- IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type -- MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type -- ANGLE" z))))

;; same as before
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

;; Constructors for complex numbers

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

;;;SECTION 2.4.3

;; uses get/put (from 3.3.3) -- see ch2support.scm

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;;footnote
;: (apply + (list 1 2 3 4))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types -- APPLY-GENERIC"
            (list op type-tags))))))

;; Generic selectors

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))


;; Constructors for complex numbers

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))



;; EXERCISE 2.73
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else (error "unknown expression type -- DERIV" exp))))


(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))

(define (operator exp) (car exp))

(define (operands exp) (cdr exp))

;: ((get (operator exp) 'deriv) (operands exp) var)


;; Message passing
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

;;;SECTION 2.5.1

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))

  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


;; EXERCISE 2.77
;; to put in complex package

;: (put 'real-part '(complex) real-part)
;: (put 'imag-part '(complex) imag-part)
;: (put 'magnitude '(complex) magnitude)
;: (put 'angle '(complex) angle)


;;;SECTION 2.5.2

;; to be included in the complex package
;: (define (add-complex-to-schemenum z x)
;:   (make-from-real-imag (+ (real-part z) x)
;:                        (imag-part z)))
;:
;: (put 'add '(complex scheme-number)
;:      (lambda (z x) (tag (add-complex-to-schemenum z x))))


;; Coercion

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

;: (put-coercion 'scheme-number 'complex scheme-number->complex)


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types"
                                (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

;; EXERCISE 2.81

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
;: (put-coercion 'scheme-number 'scheme-number
;:               scheme-number->scheme-number)
;: (put-coercion 'complex 'complex complex->complex)

(define (exp x y) (apply-generic 'exp x y))
;: (put 'exp '(scheme-number scheme-number)
;:      (lambda (x y) (tag (expt x y))))

;;;SECTION 2.5.3

;;; ALL procedures in 2.5.3 except make-polynomial
;;; should be inserted in install-polynomial-package, as indicated

(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))

(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- MUL-POLY"
             (list p1 p2))))

;; *incomplete* skeleton of package
(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  ;;[procedures same-variable? and variable? from section 2.3.2]

  ;; representation of terms and term lists
  ;;[procedures adjoin-term ... coeff from text below]

  ;;(define (add-poly p1 p2) ... )
  ;;[procedures used by add-poly]

  ;;(define (mul-poly p1 p2) ... )
  ;;[procedures used by mul-poly]

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))

  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1)) (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms L1)
                              (rest-terms L2)))))))))

(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))


;; Representing term lists

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))

(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))


;; Constructor
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))


;; EXERCISE 2.91

(define (div-terms L1 L2)
  (if (empty-termlist? L1)
      (list (the-empty-termlist) (the-empty-termlist))
      (let ((t1 (first-term L1))
            (t2 (first-term L2)))
        (if (> (order t2) (order t1))
            (list (the-empty-termlist) L1)
            (let ((new-c (div (coeff t1) (coeff t2)))
                  (new-o (- (order t1) (order t2))))
              (let ((rest-of-result
                     ??FILL-THIS-IN?? ;compute rest of result recursively
                     ))
                ??FILL-THIS-IN?? ;form complete result
                ))))))


;; EXERCISE 2.93
;: (define p1 (make-polynomial 'x '((2 1)(0 1))))
;: (define p2 (make-polynomial 'x '((3 1)(0 1))))
;: (define rf (make-rational p2 p1))


;; Rational functions

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (gcd-terms a b)
  (if (empty-termlist? b)
      a
      (gcd-terms b (remainder-terms a b))))


;; EXERCISE 2.94
;: (define p1 (make-polynomial 'x '((4 1) (3 -1) (2 -2) (1 2))))
;: (define p2 (make-polynomial 'x '((3 1) (1 -1))))
;: (greatest-common-divisor p1 p2)


;; EXERCISE 2.97

(define (reduce-integers n d)
  (let ((g (gcd n d)))
    (list (/ n g) (/ d g))))

;: (define p1 (make-polynomial 'x '((1 1)(0 1))))
;: (define p2 (make-polynomial 'x '((3 1)(0 -1))))
;: (define p3 (make-polynomial 'x '((1 1))))
;: (define p4 (make-polynomial 'x '((2 1)(0 -1))))

;: (define rf1 (make-rational p1 p2))
;: (define rf2 (make-rational p3 p4))

;: (add rf1 rf2)
