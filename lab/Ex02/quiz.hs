module Quiz where
import Test.QuickCheck
import Data.List
import Data.Char

---Q1:1,2,4,5,7
rot13 :: String -> String
rot13 = map $ \x -> 
          case lookup x table of
            Just y  -> y 
            Nothing -> x
  where
    table = table' 'A' 'Z' ++ table' 'a' 'z'
    table' a z = zip [a..z] (drop 13 (cycle [a..z]))

list = table
    where
        table = table' 'A' 'Z' ++ table' 'a' 'z'
        table' a z = zip [a..z] (drop 13 (cycle [a..z]));
--pass
prop1 :: String -> Bool
prop1 xs = 
    length xs == length (rot13 xs)
--pass
prop2:: String -> Bool
prop2 x = 
    rot13 (map toUpper x) == map toUpper (rot13 x)
--fail
prop3 :: (Char -> Char) -> [Char] -> Bool
prop3 f x =
    rot13 (map f x) == map f (rot13 x)

counter3 :: Char -> Char
counter3 x 
    | x == 'a' = 'b'
    | otherwise = x

-- pass
prop4 :: String -> Property
prop4 x = 
    all (not . isAlpha) x ==> rot13 x == x

--pass
prop5::String->String -> Bool
prop5 a b = 
    rot13 (a ++ b) == rot13 a ++ rot13 b

--fail
prop6 :: String -> Property
prop6 x = 
    not (null x) ==> ord (head x) + 13 == ord (head (rot13 x))
--pass
prop7 :: String -> Bool
prop7 x = 
    rot13 (rot13 x) == x

--- end of Q1
--- Q2 : 1,3,6
merge :: (Ord a) => [a] -> [a] -> [a]
merge (x:xs) (y:ys) | x < y     = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys
merge xs [] = xs
merge [] ys = ys
--pass
mergeProp1::[Int] -> [Int] -> Bool
mergeProp1 a b = 
    merge (sort a) (sort b) == sort (merge a b)

--fail
mergeProp2::[Int]->[Int]->Bool
mergeProp2 a b =
    merge a b == sort (a ++ b)
--pass
mergeProp3 :: [Int] -> [Int] -> Bool
mergeProp3 a b =
    length (merge a b) == length a + length b

-- fail
mergeProp4 :: (Int->Bool) -> [Int] -> [Int] -> Bool
mergeProp4 f a b = 
    merge (filter f a) (filter f b) == filter f (merge a b)
couter a = if a < 5 then True else False

--fail
mergeProp5 :: (Int -> Bool) -> [Int] -> [Int] -> Bool
mergeProp5 f a b = 
    merge (map f a) (map f b) == map f (merge a b)

--pass
mergeProp6::[Int] -> [Int] -> Bool
mergeProp6 a b =
    sort (merge a b) == sort (a ++ b)
-----------------------------------------------------------------------------------
--Q3:1,2,4,5
toBinary :: Int -> String
toBinary 0 = ""
toBinary n = let (d,r) = n `divMod` 2
              in toBinary d 
                   ++ if r == 0 then "0"
                                else "1"

fromBinary :: String -> Int
fromBinary = fst . foldr eachChar (0,1)
  where
    eachChar '1' (sum, m) = (sum + m, m*2)
    eachChar _   (sum, m) = (sum    , m*2)
--pass
binaryProp1 :: Int -> Property
binaryProp1 i = 
    i >= 0 ==> fromBinary (toBinary i) == i
--pass
binaryProp2 :: String -> Property
binaryProp2 s = 
    all (`elem` "01") s ==> toBinary (fromBinary s) == s
--fail
binaryProp3 :: String -> Property
binaryProp3 s = 
    all (`elem` "01") s ==> read s >= fromBinary s
--pass
binaryProp4 :: Int -> Property
binaryProp4 i =
    i > 0 ==> length (toBinary i) >= length (show i)
--pass
binaryProp5 :: String -> Property
binaryProp5 s = 
    all (`elem` "01") s ==> fromBinary s == fromBinary ('0':s)
-------------------------------------------------------------------------------------
--Q4:1,2,3,6 
dedup :: (Eq a) => [a] -> [a]
dedup (x:y:xs) | x == y = dedup (y:xs)
               | otherwise = x : dedup (y:xs)
dedup xs = xs

sorted :: (Ord a) => [a] -> Bool
sorted (x:y:xs) = x <= y && sorted (y:xs)
sorted xs = True
--pass
dedupProp1 :: String-> Property
dedupProp1 xs = 
    sorted xs ==> sorted (dedup xs)
--pass
dedupProp2 :: String -> Property
dedupProp2 xs =
    sorted xs ==> dedup xs == nub xs

--pass
dedupProp3 :: String -> Property
dedupProp3 xs =
    sorted xs ==> dedup (dedup xs) == dedup xs
-- FAIL
dedupProp4 :: [Int] -> [Int]-> Property
dedupProp4 xs ys = 
    sorted xs && sorted ys ==> dedup xs ++ dedup ys == dedup (xs ++ ys)
--fail
dedupProp5 :: String -> Property
dedupProp5 xs = 
    sorted xs ==> length (dedup xs) < length xs
--pass
dedupProp6 :: Char -> String -> Bool
dedupProp6 x xs = 
    (x `elem` xs) == (x `elem` dedup xs)

-----------------------------------------------------------------
--Q5: 4
foo :: [a] -> (a -> b) -> [b]

-----implementation
-- foo xs f = [] --1. fail at prop1
-- foo xs f = xs --2. fail at type check
-- foo [] f = [] -- 3.fail at type check
-- foo (x:xs) f = x : foo xs f

-- foo [] f = []  -- 5. fail at prop 1
-- foo (x:xs) f = foo xs f

foo [] f = [] -- most correct, but die in my counter example
foo (x:xs) f = f x : foo xs f

-- prop_1 :: [Int] -> Bool
-- prop_1 xs = foo xs id == xs 

-- prop_2 :: [Int]->(Int -> Int) -> (Int -> Int)  -> Bool 
-- prop_2 xs f g = foo (foo xs f) g == foo xs (g . f)

--this is counterexample function
q5::Int->Int
q5 i = maxBound --ASK question: how to let quickcheck generate data for first argument, since succ for Int is a partial function. in this 
                -- quiz, all the function assume as total function



---------------------------------------------------------------
--Q6:2,4
--3 fail at 1.
--1 fail at 1.
--5,6 too easy to fail
bar :: [Int] -> [Int]
bar xs = go xs []
  where go []     acc = acc
        go (x:xs) acc = go xs (x:acc)

-- bar = id


-- q6 i = maxBound - i
-- prop_1 :: [Int] -> Bool
-- prop_1 xs = bar (bar xs) == xs

-- prop_2 :: [Int] -> Bool
-- prop_2 xs = length xs == length (bar xs)

-- prop_3 ::  (Int -> Int) -> [Int] -> Bool
-- prop_3 f xs= bar (map f xs) == map f (bar xs)

-------------------------------------------
--Q7:1
--2 fail at 3,
--3 fail at 1. (empty list)
--4 fail at 3
baz :: [Integer] -> Integer
baz = foldr (+) 0


-- prop_1 :: [Integer] -> [Integer] -> Bool
-- prop_1 xs ys = baz xs + baz ys == baz (xs ++ ys)

-- prop_2 :: [Integer] -> Bool
-- prop_2 xs = baz xs == baz (reverse xs) 

-- prop_3 :: Integer -> [Integer] -> Bool 
-- prop_3 x xs = baz (x:xs) - x == baz xs



---------------------------------------
--Q8:3
fun :: [Integer] -> [Integer]
fun []       = []
fun [x]      = []
fun (x:y:xs) = (y-x):fun (y:xs)


--1 fail at 1
--1,2,4 fail at 1,2.
--5 fail at 1,2
nuf :: [Integer] -> Integer -> [Integer]

nuf xs i = scanl (\v x -> v + x) i xs 

prop_1 :: [Integer] -> Integer -> Bool
prop_1 xs x = nuf (fun (x:xs)) x == (x:xs)

prop_2 :: [Integer] -> Integer -> Bool
prop_2 xs x = fun (nuf xs x) == xs