module Lec2(
    foldLeft
)where

--Quicksort


qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
                where smaller = filter (\a -> a <= x) xs
                      larger = filter (\a -> a > x) xs
-- list operation
sum' ::[Int] -> Int
-- sum, concat, map, filter, folder, foldl
sum' [] = 0
sum' (x:xs) = sum' xs + x

-- foldr
foldRight :: (a -> b -> b) -> b -> [a] -> b
foldRight f e [] = e
foldRight f e (x:xs) = f x $ foldRight f e xs

--map
map' :: (a->b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map f xs

map'' :: (a->b) -> [a] -> [b]
map'' f = foldRight ((:) . f) []

filter' :: (a->Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs) = if f x
                    then x : filter' f xs
                    else filter' f xs

filter'':: (a -> Bool) -> [a] -> [a]
filter'' p = foldRight (\a ys-> if p a then a : ys else ys) []

--foldLeft
foldLeft :: (t1 -> t2 -> t1) -> t1 -> [t2] -> t1
foldLeft f e [] = e
foldLeft f e (x:xs) = foldLeft f (f e x) xs

--reverse
reverse'' :: [a]->[a]
reverse'' = foldRight(\a bs-> bs ++ [a]) []

reverse3 :: [a] -> [a]
reverse3 = foldLeft (\xs y -> y:xs ) []


units :: [String]
units = ["zero","one","two","three","four","five","six","seven","eight","nine"]

add = (5 - )
