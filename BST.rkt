#lang racket
(require test-engine/racket-tests)

;; purely functional binary search tree
;; a tree is composed of a root, a left subtree, and a right subtree
;; all leafs have two dummy nodes as lt and rt.

;; constructors, helpers

(define make-terminal-node
  (lambda ()
    null))

(define make-tree
  (lambda (data lt rt) 
    (list data lt rt)))

(define make-leaf
  (lambda (data)
    (make-tree data 
               (make-terminal-node)
               (make-terminal-node))))

(define (make-empty-tree)
  (make-leaf (make-terminal-node)))

;; add a list of stuff into an existing bst
(define insert-list
  (lambda (bst lst)
    ;; can't figure out how to modify insert s.t. the args line up,
    ;; here's a hack
    (let ((flip (λ (f) (λ (x y) (f y x)))))
      (foldl (flip insert) bst lst))))

;; turn a list into a bst
;; cdr down a list, insert cars into a bst in order
(define list->bst
  (lambda (lst)
    (insert-list (make-empty-tree) lst)))

;; serialise a tree into a list with dfs
;; already stored in a dfs order!
(define dfs flatten)
;; same thing
(define bst->list dfs)

;; accessors, queries
; should this be a macro?
(define safe-access
  (lambda (fx)
    (lambda (bst)
      (if (is-empty? bst) bst
          (fx bst)))))

(define root (safe-access car))
(define left-tree (safe-access cadr))
(define right-tree (safe-access caddr))

;; min is leftmost node
(define min
  (lambda (bst)
    (if (is-empty? (left-tree bst)) 
        (root bst)
        (min (left-tree bst)))))

;; max is rightmost node
(define max
  (lambda (bst)
    (if (is-empty? (right-tree bst)) 
        (root bst)
        (max (right-tree bst)))))

(define is-terminal-node? null?)

(define is-empty-tree?
  (lambda (bst)
    (equal? bst (make-empty-tree))))
  
(define is-empty? 
  (lambda (bst) 
    (or (is-terminal-node? bst)
        (is-empty-tree? bst))))

(define is-leaf?
  (lambda (bst)
    (and (is-terminal-node? (left-tree bst))
         (is-terminal-node? (right-tree bst)))))

(define is-member?
  (lambda (bst item)
    (cond
      ((is-empty? bst) #f)
      ((eq? item (root bst)) #t)
      ((< item (root bst)) (is-member? (left-tree bst) item))
      ((> item (root bst)) (is-member? (right-tree bst) item)))))

;; operations on the tree
;; insert, remove
;; higher order: tree-map, tree-fold, tree-filter

(define insert
  (lambda (bst item)
    (cond
      ;; if the tree is null, then make a new bst with the item at the root
      ((is-empty? bst) (make-leaf item))
      ;; if the item is smaller than the root, try and add it into the left subtree
      ((< item (root bst))
       (make-tree (root bst)
                  (insert (left-tree bst) item)
                  (right-tree bst)))
      ;; if the item is greater than the root, try and add it into the right subtree
      ((> item (root bst))
       (make-tree (root bst)
                  (left-tree bst)
                  (insert (right-tree bst) item)))
      ;; if the value is inside the tree already just return the bst.
      (else bst))))

(define remove
  (lambda (bst item)
    (cond
      ;; if the tree is empty, nothing to remove
      ((is-empty? bst) bst)
      ;; if the item is smaller than the root, remove from left subtree
      ((< item (root bst))
          (make-tree (root bst)
                     (remove (left-tree bst) item)
                     (right-tree bst)))
      ;; if the item is greater than the root, remove from right subtree
      ((> item (root bst))
       (make-tree (root bst)
                  (left-tree bst)
                  (remove (right-tree bst) item)))
      ;; item found, remove
      (else (let* ((min-elt (right-tree bst)))
                  ; (succ (if (= item min-elt)))
                            
            (make-tree min-elt
                       (left-tree bst)
                       (remove (right-tree bst) min-elt)))))))
    
    
;; unit tests
(define tree1 (list->bst '(5 10 1 1000 -1000)))

(check-expect
 ;; serialise the list '(5 10 1 1000 -1000) into a tree
 (insert (insert (insert (insert (insert (make-empty-tree) 5) 10) 1) 1000) -1000)
 tree1)

(check-expect (root tree1) 5)

(check-expect (max tree1) 1000)

(check-expect (min tree1) -1000)

;; special case where we can recover the insert order
(check-expect (bst->list (list->bst '(1 2 3 4 5))) '(1 2 3 4 5))

(test)
