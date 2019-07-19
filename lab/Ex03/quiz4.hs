module Quiz4 where

import Test.QuickCheck
import Data.List
import Test.QuickCheck.Modifiers

--- Q1:1

merge :: (Ord a) => [a] -> [a] -> [a]
merge (x:xs) (y:ys) | x < y     = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys
merge xs [] = xs
merge [] ys = ys

sorted :: Ord a => [a] -> Bool
sorted (x1 : x2 : xs) = (x1 <= x2) && sorted (x2 : xs)
sorted _ = True

-- prop_1 :: OrderedList Int -> OrderedList Int -> Bool
-- prop_1 (Ordered xs) (Ordered ys) = sorted (merge xs ys)

-- prop_2 :: [Int] -> [Int] -> Bool
-- prop_2 xs ys = length (merge xs ys) == length xs + length ys

-- prop_3 :: OrderedList Int -> OrderedList Int -> Bool
-- prop_3 (Ordered xs) (Ordered ys) = merge xs ys == sort (xs ++ ys)

-- prop_4 :: Fun Int Int -> [Int] -> [Int] -> Bool
-- prop_4 (Fn f) xs ys = sort (map f (merge xs ys))
--               == sort (merge (map f xs) (map f ys))

-- prop_5 :: Fun Int Bool -> OrderedList Int -> OrderedList Int -> Bool
-- prop_5 (Fn f) (Ordered xs) (Ordered ys) = filter f (merge xs ys) 
--                                   == merge (filter f xs) (filter f ys)


----- Q2: 3
{-
    prop_1 cover prop_3.
-}
rev :: [Int] -> [Int]
rev (x:xs) = rev xs ++ [x]
rev []     = []

prop_1 :: [Int] -> [Int] -> Bool
prop_1 xs ys = rev (xs ++ ys) == rev ys ++ rev xs

prop_2 :: [Int] ->  Bool
prop_2 xs = length xs == length (rev xs)

prop_3 :: [Int] -> Int -> Bool
prop_3 xs x = count x xs == count x (rev xs)
  where
    count x xs = length (filter (== x) xs)

-- Q3: 1,3
-- 2,4 are pointless
type Graph = [[Bool]]

m :: Graph
m = [[False, True,  True,  False],
     [True,  False, False, True ],
     [True,  False, False, True ],
     [False, True,  True,  False]]


newGraph :: Int -- number of nodes
    -> Graph
newGraph n = replicate n (replicate n False)

connected :: Graph -> (Int, Int) -> Bool
connected g (x, y) 
  | x < length g && y < length g = (g !! x) !! y 
  | otherwise                    = False

-- connect :: (Int, Int) -> Graph -> Graph
-- connect (x, y) g = modify x (modify y (\_ -> True)) g
--   where
--     modify :: Int -> [a] -> (a -> a) -> [a]
--     modify 0 (x:xs) f = f x : xs
--     modify n (x:xs) f = x : modify (n - 1) xs f
--     modify n []     f = []
-- Q4: 1,4
-- 2 should be another way around
-- 3 should have input is wellformed


-----Q5 : 4. bu fei hua.
data Model = M Int [(Int, Int)] 
                deriving (Show, Eq)

instance Arbitrary Model where
  arbitrary = sized create
    where 
      create 0 = return (M 0 [])
      create n =
        M n <$> fmap nub (listOf $ elements [(x,y) | x <- [0..n-1], y <- [0..n-1]])

newGraphA :: Int -> Model
newGraphA n = M n []

connectedA :: Model -> (Int, Int) -> Bool
connectedA (M n es) (x,y) = (x,y) `elem` es

connectA :: (Int, Int) -> Model -> Model
connectA (x, y) (M n es) 
  | x < n && y < n = M n ((x,y):(y,x):es)
  | otherwise      = M n es 

------------Q6: 1
toConcrete :: Model -> Graph
toConcrete (M n es) 
    = map (\x -> map (\y -> (x,y) `elem` es) [0..n-1]) [0..n-1]

toAbstract :: Graph -> Model 
toAbstract g = let n = length g 
                in M n $ filter (\(x,y) -> (g !! x ) !! y) 
                                [(x,y) | x <- [0..n-1], y <- [0..n-1]]


----Q7:1 bu fei hua

---Q8:5 bu fei hua: good question to do revision

prop_temp_1 n = n >= 0 ==>
  toAbstract (newGraph n) == newGraphA n





---------------------------------------------- quzi 6
data NonEmptyList1 a = One a | Cons a (NonEmptyList1 a) deriving Eq

instance Functor NonEmptyList1 where
  fmap f (One x) = One (f x)
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)
  
-- instance Applicative NonEmptyList1 where
--   pure x = One x
--   One f <*> xs = fmap f xs 
--   (Cons f fs) <*> xs = fmap f xs `append` (fs <*> xs)
--     where
--       append (One x) ys = Cons x ys
--       append (Cons x xs) ys = Cons x (xs `append` ys)

instance Applicative NonEmptyList1 where
  pure x = Cons x (pure x)
  (One f) <*> (One x) = One (f x)
  (One f) <*> (Cons x _) = One (f x)
  (Cons f _) <*> (One x) = One (f x)
  (Cons f fs) <*> (Cons x xs) = Cons (f x) (fs <*> xs)

prop_applicative2 :: Eq b => Fun a b -> a -> Bool
prop_applicative2 (Fn f) x = (pure f <*> (pure :: a -> NonEmptyList1 a) x) == pure (f x)