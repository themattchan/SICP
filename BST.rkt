#lang racket

;; purely functional binary search tree

;; constructors

(define make-empty-tree
  (lambda () null))

(define make-tree
  (lambda (root lt rt) 
    (list root lt rt)))

;; queries

(define is-empty? 
  (lambda (bst) 
    (null? bst)))

(define is-member?
  (lambda (bst item)
    (cond
      ((is-empty? bst) #f)
      ((eq? item (root bst)) #t)
      ((< item (root bst)) (is-member? left-tree item))
      ((> item (root bst)) (is-member? right-tree item)))))

;;max is rightmost node
(define max
  (lambda (bst)
    (if (is-empty? (right-tree bst)) (root bst)
        (max (right-tree bst)))))

;;min is leftmost node
(define min
  (lambda (bst)
    (if (is-empty? (left-tree bst)) (root bst)
        (min (left-tree bst)))))

;; accessors
;; a tree is composed of a root, a left subtree, and a right subtree

(define root 
  (lambda (bst) 
    (if (is-empty? bst) '()
        (car bst))))

(define left-tree
  (lambda (bst) 
    (car (cdr bst))))

(define right-tree
  (lambda (bst) 
    (last bst)))

;; operations on the tree

(define insert
  (lambda (bst item)
    (cond
      ;; if the tree is null, then make a new bst with the item at the root
      ((is-empty? bst)
       (make-tree item (make-empty-tree) (make-empty-tree)))
      ;; if the item is smaller than the root, try and add it into the left subtree
      ((< item (root bst))
       (make-tree (root bst)
                  (insert (left-tree bst) item)
                  (right-tree bst)))
      ;; if the item is smaller than the root, try and add it into the right subtree
      ((> item (root bst))
       (make-tree (root bst)
                  (left-tree bst)
                  (insert (right-tree bst) item)))
      ;; if the value is inside the tree already just return the bst.
      ;; don't test for membership first cause that's costly
      (bst))))

; (define remove
;   (lambda (bst item)
; 	;; only remove if the item is actually in the tree
; 	(if (is-member? bst item)
; 		()
; 	 ;; else, tree is unchanged
; 	 bst)))
; 	


(define tr (make-empty-tree))
(define tree (insert (insert (insert (insert (insert tr 5) 10) 1) 1000) -1000))
tree
(is-empty? '())
(is-member? tree -100)
