SICP Chapter 2: Selected Questions + Solutions in Haskell
=========================================================

Data Abstraction
----------------

1. Develop a data definition for rational numbers

> {-@ Rat :: Int -> {d:Int | d /= 0} -> Rat @-}
> data Rat = Rat { num :: Int, denom :: Int }
>          deriving (Show, Eq)

2, 3. Implement a representation for points and rectangles in a cartesian plane.

4+5. (Implement `car, cons, cdr`. Do in Scheme)

6. Church numerals.

We can implement zero as

> zero = \f -> \x -> x

and successor as

> succ n = \f -> \x -> (f ((n f) x))

Implement `one` and `two` directly, then implement `add` which adds two Church>
numerals.

Lists
-----

17. Write a function to get the last element of a List

> llast :: [a] -> a
> llast [x]    = x
> llast (x:xs) = llast xs

18. Write reverse

> rev :: [a] -> [a]
> rev []     = []
> rev (x:xs) = rev xs ++ [x]

19. Refactor the count-change procedure to accept different currencies, to be given as
    lists of ints. Specifically, change the type of `cc :: Int -> Int -> Int` to
    `cc :: Int -> [Int] -> Int`

\begin{spec}
countChange :: Int -> Int
countChange amount = cc amount 5
  where
    cc :: Int -> Int -> Int
    cc amount kindsOfCoins
     | amount == 0       = 1
     | amount < 0        = 0
     | kindsOfCoins == 0 = 0
     | otherwise         = (cc amount (kindsOfCoins - 1)) +
                           (cc (amount - firstDenomination kindsOfCoins) kindsOfCoins)
    firstDenomination kindsOfCoins = case kindsOfCoins of
        1 -> 1
        2 -> 5
        3 -> 10
        4 -> 25
        5 -> 50
\end{spec}

> countChange :: Int -> [Int] -> Int
> countChange _ [] = 0
> countChange amount (c:cs)
>  | amount == 0 = 1
>  | amount < 0  = 0
>  | otherwise   = (countChange amount cs) +
>                  (countChange (amount - c) cs)

21. Write a function to square a list of ints using recursion, and using map.

> sqrec []     = []
> sqrec (x:xs) = x*x : sqrec xs
>
> sq xs = map (\x -> x*x) xs

23. Write `foreach :: (a -> IO ()) -> [a] -> IO ()`

> foreach :: (a -> IO ()) -> [a] -> IO ()
> foreach f []       = return ()
> foreach f (io:ios) = f io >> foreach f ios

Trees
-----

Here is where the differences start to come into play. This section relies on
hetrogeneous lisp lists, which Haskell doesn't have. Instead, this turns into
another exercise in data abstraction.

Here is a Lisp style tree.

> data Tree a = Tree [Tree a] | Leaf a
>             deriving Show

27. (IMPOSSIBLE) Deep reverse a (possibly infinitely) nested list of lists

28. Write a procedure `fringe` that takes a tree and returns a list whose
    elements are all leaves of the tree arranged in left-to-right order.

> fringe :: Tree a -> [Tree a]
> fringe l@(Leaf x)  = [l]
> fringe   (Tree xs) = concatMap fringe xs

> test28 = fringe $ Tree [Tree [Leaf 1, Leaf 2], Tree [Leaf 3, Leaf 4]]
> -- [Leaf 1,Leaf 2,Leaf 3,Leaf 4]

29.
     *Exercise 2.29:* A binary mobile consists of two branches, a left
     branch and a right branch.  Each branch is a rod of a certain
     length, from which hangs either a weight or another binary mobile.
     We can represent a binary mobile using compound data by
     constructing it from two branches (for example, using `list'):

          (define (make-mobile left right)
            (list left right))

     A branch is constructed from a `length' (which must be a number)
     together with a `structure', which may be either a number
     (representing a simple weight) or another mobile:

          (define (make-branch length structure)
            (list length structure))

       a. Write the corresponding selectors `left-branch' and
          `right-branch', which return the branches of a mobile, and
          `branch-length' and `branch-structure', which return the
          components of a branch.

       b. Using your selectors, define a procedure `total-weight' that
          returns the total weight of a mobile.

       c. A mobile is said to be "balanced" if the torque applied by
          its top-left branch is equal to that applied by its top-right
          branch (that is, if the length of the left rod multiplied by
          the weight hanging from that rod is equal to the
          corresponding product for the right side) and if each of the
          submobiles hanging off its branches is balanced. Design a
          predicate that tests whether a binary mobile is balanced.

       d. Suppose we change the representation of mobiles so that the
          constructors are

               (define (make-mobile left right)
                 (cons left right))

               (define (make-branch length structure)
                 (cons length structure))

          How much do you need to change your programs to convert to
          the new representation?

See *Why Calculating is better than Scheming", Wadler. Much simpler in Haskell.

30. Write a function to square a tree of ints.

31. Write `treemap` for the above tree datatype.

> treemap :: (a -> b) -> Tree a -> Tree b
> treemap f (Leaf x)  = Leaf $ f x
> treemap f (Tree xs) = Tree $ map (treemap f) xs
>
> instance Functor Tree where
>   fmap = treemap
>
> test29 = fmap (+1) t
>   where t = Tree [Tree [Leaf 1, Leaf 2], Tree [Leaf 3, Leaf 4]]

32. Write a function to generate all subsets of a list. E.g.
    `subsets [1,2,3] => [[],[3],[2],[2,3],[1],[1,3],[1,2],[1,2,3]]`

> subsets :: [a] -> [[a]]
> subsets []     = [[]]
> subsets (x:xs) = let rest = subsets xs in
>                    rest ++ map (x:) rest
 
