module Main where

nb :: Float
nb = 2.0

main :: IO ()
--main = putStrLn(show (polyEval nb [1.0, 2.0, 3.0]))
main = putStrLn(show (polyAdd [1.0, 2.0, 3.0] [2.0, 3.0]))


polyEvalRec :: (Num a) => a -> [a] -> a
polyEvalRec _ [] = 0
polyEvalRec x (y:ys) = y + x * polyEvalRec x ys

polyEval :: (Num a) => a -> [a] -> a
polyEval x = foldr (\z n -> z + x * n ) 0

polyAddRec :: (Num a) => [a] -> [a] -> [a]
polyAddRec [] [] = []
polyAddRec (x:xs) [] = x:xs
polyAddRec [] (y:ys) = y:ys
polyAddRec (x:xs) (y:ys) = (x+y) : polyAddRec xs ys

polyAdd :: (Num a) => [a] -> [a] -> [a]
polyAdd xs ys 
  | l > 0 = map (\(x,y) -> x+y) (zip xs (ys++(replicate l 0))) 
  | l < 0 = map (\(x,y) -> x+y) (zip (xs++(replicate l 0)) ys)
  | True = map (\(x,y) -> x+y) (zip xs ys)
    where l = length xs - length ys

         
polyAdd2 :: (Num a) => [a] -> [a] -> [a]
polyAdd2 = zipWith (+)

polyMult :: (Num a) => [a] -> [a] -> [a]
polyMult [] _ = []
polyMult (x:xs) ys = polyAddRec (map (*x) ys) (0 : polyMult xs ys) 