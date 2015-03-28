SICP Chapter 2: Selected Questions + Solutions in Haskell
=========================================================

> module SICP2 where

Data Abstraction
----------------

1. Develop a data definition for rational numbers

> {-@ Rat :: Int -> {d:Int | d /= 0} -> Rat @-}
> data Rat = Rat { num :: Int, denom :: Int }
>          deriving (Show, Eq)

2. Implement a representation for points and lines

> type Point = (Int, Int)
> data Line  = Line Point Point
>
> midpoint :: Line -> Point
> midpoint (Line (x1,y1) (x2,y2)) = ((x1+x2) `div` 2, (y1+y2) `div` 2)
>
> lineLength :: Line -> Double
> lineLength (Line (x1,y1) (x2,y2)) = sqrt . fromIntegral $ (x2-x1)^2 + (y2-y1)^2

3. ... and rectangles in the cartesian plane.

> data Rect1 = Rect1 { tl::Point       -- top left corner
>                    , br::Point       -- bottom right corner
>                    }
>
> data Rect2 = Rect2 { c::Point      -- top left corner
>                    , w::Int        -- width
>                    , h::Int        -- height
>                    }
>
> area1 (Rect1 (x1,y1) (x2,y2)) = lineLength (Line (x1,y1) (x2,y1)) *
>                                 lineLength (Line (x1,y1) (x1,y2))
>
> area2 (Rect2 _ w h) = w*h

4. Implement cons, car, cdr using functions only

> cons x y = \m -> m x y
> car z    = z (\p q -> p)
> cdr z    = z (\p q -> q)

6. Church numerals.

We can implement zero as

< zero = \f -> \x -> x

and successor as

< succ n = \f -> \x -> (f ((n f) x))

Implement `one` and `two` directly, then implement `add` which adds two Church
numerals.

> one = \f -> \x -> f x
> two = \f -> \x -> f (f x)
>
> addChurch m n = \f -> \x -> ((m f) ((n f) x))

Data-directed programming
-------------------------

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

21. Write a function to square a list of ints using recursion, and again using map.

> sqrec :: [Int] -> [Int]
> sqrec []     = []
> sqrec (x:xs) = x*x : sqrec xs
>
> sq :: [Int] -> [Int]
> sq = map (\x -> x*x)

23. Write `foreach :: (a -> IO ()) -> [a] -> IO ()`

> foreach :: (a -> IO ()) -> [a] -> IO ()
> foreach f []       = return ()
> foreach f (io:ios) = f io >> foreach f ios

Trees
-----

Here is a tree/Lisp-list datatype. (Not quite, Lisp lists are hetrogeneous.)

> data Tree a = Tree [Tree a] | Leaf a
>             deriving Show

27. Deep reverse a (possibly infinitely) nested list of lists
   (Hint: represent this as a tree.)

`((1 2) (3 4)) => ((4 3) (2 1))`

> deepRev :: Tree a -> Tree a
> deepRev l@(Leaf x)  = l
> deepRev   (Tree xs) = Tree $ map deepRev (reverse xs)
>
> test27 = deepRev $ Tree [Tree [Leaf 1, Leaf 2], Tree [Leaf 3, Leaf 4]]

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

(See *Why Calculating is better than Scheming*, Wadler.)

> type Weight = Int
> type Length = Int
> data Mobile = M  Branch Branch deriving Show
> data Branch = BW Length Weight
>             | BM Length Mobile
>             deriving Show
>
> totalWeight :: Mobile -> Int
> totalWeight (M l r) = weight l + weight r
>   where weight (BW _ w) = w
>         weight (BM _ m) = totalWeight m
>
> isBalanced :: Mobile -> Bool
> isBalanced (M l r) = torque l == torque r && balSub l && balSub r
>   where torque (BW l w) = l * w
>         torque (BM l m) = l * totalWeight m
>         balSub (BW _ _) = True
>         balSub (BM _ m) = isBalanced m
>
> test29_Mobile = M (BM 2 (M (BW 2 3)
>                            (BW 2 3)))
>                   (BW 2 3)
> test29_1 = totalWeight test29_Mobile
> test29_2 = isBalanced test29_Mobile

Because we don't have accessor functions, changes in the data definition will
force updates in all pattern matches, which is less versatile. But, we do get
assistance from the type system. And, there's really only one way to do this
problem...


30. Write a function to square a tree of ints.

> sqTree :: Tree Int -> Tree Int
> sqTree (Leaf x)  = Leaf $ x*x
> sqTree (Tree xs) = Tree $ map sqTree xs

31. Generalise #30 and write `treemap` for the above tree datatype.

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
 
Higher-order functions
----------------------

Extra: Implement foldl and foldr

> ffoldl :: (b -> a -> b) -> b -> [a] -> b
> ffoldl f a []     = a
> ffoldl f a (x:xs) = ffoldl f (f a x) xs
>
> ffoldr :: (a -> b -> b) -> b -> [a] -> b
> ffoldr f a []     = a
> ffoldr f a (x:xs) = f x (ffoldr f a xs)

33. Implement map, append, and length with fold.

> accumulate = ffoldr
>
> mmap :: (a -> b) -> [a] -> [b]
> mmap f xs = accumulate (\x a -> f x : a) [] xs
>
> aappend :: [a] -> [a] -> [a]
> aappend xs ys = accumulate (:) ys xs
>
> llength :: [a] -> Int
> llength xs = accumulate (const (+1)) 0 xs

34. Horner's rule.

    *Exercise 2.34:* Evaluating a polynomial in x at a given value of
     x can be formulated as an accumulation.  We evaluate the polynomial

          a_n r^n | a_(n-1) r^(n-1) + ... + a_1 r + a_0

     using a well-known algorithm called "Horner's rule", which
     structures the computation as

          (... (a_n r + a_(n-1)) r + ... + a_1) r + a_0

     In other words, we start with a_n, multiply by x, add a_(n-1),
     multiply by x, and so on, until we reach a_0.(3)

     Fill in

     < horner :: Int -> [Int] -> Int
     < horner x coeffs = accumulate (\t a -> ????) 0 coeffs

> horner :: Int -> [Int] -> Int
> horner x coeffs = accumulate (\t a -> t*x + a) 0 coeffs

35. Count leaves using fold. (First define treefold.)

> treefoldr :: (Tree a -> b -> b) -> b -> Tree a -> b
> treefoldr f b t = accumulate f b (fringe t)
>
> test35 = treefoldr f 0 t
>   where f (Leaf a) b = a + b
>         t = Tree [Tree [Leaf 1, Leaf 2], Tree [Leaf 3, Leaf 4]]
>
> countLeaves :: Tree a -> Int
> countLeaves = treefoldr f 0
>   where f (Leaf _) b = 1 + b

36. Define `foldn` which takes as input a matrix (list of lists) and folds using
    the transpose of that matrix.

   E.g. `foldn + 0 [[1,2,3],[4,5,6],[7,8,9],[10,11,12]] => [22, 26, 30]`

> foldn :: (a -> b -> b) -> b -> [[a]] -> [b]
> foldn f b ([]:_) = []
> foldn f b xs     = foldr f b (map head xs) :
>                    foldn f b (map tail xs)

37. A matrix is a list of lists, where each inner list is a row. (A vector is
    represented as a list). Implement the matrix operations.

\begin{spec}
type Matrix = [[Int]]
type Vector = [Int]

dotProduct :: Vector -> Vector -> Int
dotProduct v w = accumulate (+) 0 (zipWith (*) v w)

matrixTimesVector :: Matrix -> Vector -> Vector
matrixTimesVector m v = map ????  m

transpose :: Matrix -> Matrix
transpose m = foldn ???? ???? m

matrixTimesMatrix :: Matrix -> Matrix -> Matrix
matrixTimesMatrix m n = let cols = transpose n in
                           map ????  m

\end{spec}

> type Matrix = [[Int]]
> type Vector = [Int]
>
> dotProduct :: Vector -> Vector -> Int
> dotProduct v w = accumulate (+) 0 (zipWith (*) v w)
>
> matrixTimesVector :: Matrix -> Vector -> Vector
> matrixTimesVector m v = map (dotProduct v) m
>
> transpose :: Matrix -> Matrix
> transpose m = foldn (:) [] m
>
> matrixTimesMatrix :: Matrix -> Matrix -> Matrix
> matrixTimesMatrix m n = let cols = transpose n in
>                            map (matrixTimesVector cols) m
>

39. Write reverse in terms of foldl and foldr

> reverseL :: [a] -> [a]
> reverseL = foldl (\a x -> x:a) []
>
> reverseR :: [a] -> [a]
> reverseR = foldr (\x a -> a ++ [x]) []

40.

> {-@ test :: {l:[a] | len l > 0}@-}
> test = []
