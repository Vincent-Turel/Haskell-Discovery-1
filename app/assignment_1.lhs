CS 403: Assignment 1

Vincent Turel
Thomas DiGrande
Julien Whitfield

_________________________________________________________
1. polyEval :

    - Using custom fold

> polyEval :: (Num a) => a -> [a] -> a
> polyEval x = foldr (\z n -> z + x * n ) 0

Comments :
  Works as expected.
  We define the operator in the function directly using anonymous function

Testing :
  - polyEval 2 [1.0, 2.0, 3.0]      -> 17.0
  - polyEval 3.0 [1.0, 2.0, 3.0]    -> 34.0
  - polyEval 4 []                   -> 0
  - polyEval 0 [1.0, 2.0, 3.0]      -> 1.0
_________________________________________________________
2. polyAdd :

    - Straightforward inductive version

> polyAddRec :: (Num a) => [a] -> [a] -> [a]
> polyAddRec [] [] = []
> polyAddRec (x:xs) [] = x:xs
> polyAddRec [] (y:ys) = y:ys
> polyAddRec (x:xs) (y:ys) = (x+y) : polyAddRec xs ys

    - Using 'zipWith'

> polyAdd :: (Num a) => [a] -> [a] -> [a]
> polyAdd xs ys
>   | l > 0 = zipWith (+) xs (ys++replicate l 0)
>   | l < 0 = zipWith (+) (xs++replicate (-l) 0) ys
>   | True = zipWith (+) xs ys
>     where l = length xs - length ys

Comments :
  The simple version with 'zip' and 'map' doesn't work well since 'zip' produce an array of the size of the shortest 
  given array. In order to fix this, we have decided to fill up with zeros so that the 2 arrays have the same size. 
  To accomplish that, we calculate the difference between the two and then add zeros using 'replicate' function on the smallest list. This 
  works pretty well and execution time is really close to the straithforward inductive function, even on a large differnce in list sizes
  (tested with 1 and 100.000  using ':set +s' ghci command). We have also made a 'polyAdd' version
  which uses 'zipWith' instead of 'zip' and 'map', making it a bit more consise.

Testing :
  - polyAddRec [1.0, 2.0, 3.0] [2.0, 3.0]   -> [3.0, 5.0, 3.0]
  - polyAddRec [2.0, 3.0] [1.0, 2.0, 3.0]   -> [3.0, 5.0, 3.0]
  - polyAddRec [2.0, 3.0] []                -> [2.0, 3.0]
  - polyAdd [1.0, 2.0, 3.0] [2.0, 3.0]      -> [3.0, 5.0, 3.0]
  - polyAdd [2.0, 3.0] [1.0, 2.0, 3.0]      -> [3.0, 5.0, 3.0]
  - polyAdd [2.0, 3.0] []                   -> [2.0, 3.0]
_________________________________________________________
3. polyMult

    - Using 'map' and 'polyAdd'

> polyMult :: (Num a) => [a] -> [a] -> [a]
> polyMult [] _ = []
> polyMult _ [] = []
> polyMult (x:xs) ys = polyAddRec (map (*x) ys) (0 : polyMult xs ys)

Comments :
  Works as expected.

Testing :
  - polyMult [1.0, 2.0, 3.0] [2.0, 3.0]   -> [2.0, 7.0, 12.0, 9.0]
  - polyMult [2.0, 3.0] [1.0, 2.0, 3.0]   -> [2.0, 7.0, 12.0, 9.0]
  - polyMult [2.0, 3.0] []                -> []
  - polyMult [] [2.0, 3.0]                -> []