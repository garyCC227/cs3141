module Ex02 where
import Test.QuickCheck
import Data.List
-- implement the following functions, which meet some (but not all!) of the 
-- properties of a correct sorting function

-- prop2 & 4, but not prop1 & 3 & 5
-- elem in return value, length stay the same.
dodgySort1 :: [Int] -> [Int]
dodgySort1 xs = foldr dodgySorted1 [] xs
  where
    dodgySorted1:: Int -> [Int] -> [Int]
    dodgySorted1 x [] = [x]
    dodgySorted1 x (y:ys)
      | x <= y = x : y : ys
      | otherwise = x : y : ys


-- prop1 & 2 & 3, but not prop4 & 5
--reverse and not reverse is same.
--element in the return list
--correct sorted
-- length staty not the same, and not match a proper sorting function
dodgySort2 :: [Int] -> [Int]
dodgySort2 xs = foldr dodgySorted2 [] $ (reverse.sort) xs
    where
      dodgySorted2:: Int -> [Int] -> [Int]
      dodgySorted2 x [] = [x] 
      dodgySorted2 x (y:ys) 
        | x <= y = x : ys
        | otherwise = y : dodgySorted2 x ys


-- prop1 & 3 & 4, but not prop2 & 5
-- sorted, reverse same, length same. ele not in list, not correct sort
-- final: a list with all ele are maximum num. [1,0] - > [1,1]
dodgySort3 :: [Int] -> [Int]
dodgySort3 xs = foldr dodgy3 [] xs
    where
      dodgy3::Int -> [Int] -> [Int]
      dodgy3 x [] = [x]
      dodgy3 x (y:ys)
        | x <= y = y: dodgy3 y ys
        | otherwise = x: dodgy3 x ys


-- prop1 & 2 & 3 & 4, but not prop5
dodgySort4 :: [Int] -> [Int]
dodgySort4 xs = foldr dodgy4 [] $ (reverse.sort) xs
    where
      dodgy4 x [] = [x]
      dodgy4 x (y:ys)
        | x <= y = x: (y+1): (map (+ 1) ys)
        | otherwise = y: dodgy4 x ys


-- Properties of sorting function   
-- 1.sort the list and reverse of the list. are the same 
sortProp1 :: ([Int] -> [Int]) -> [Int] -> Bool
sortProp1 sortFn xs = sortFn xs == sortFn (reverse xs)

--2.element x is in the return value list_n
--list_n = return of sortFn of (xs++[x]++ys);
sortProp2 :: ([Int] -> [Int]) -> Int -> [Int] -> [Int] -> Bool
sortProp2 sortFn x xs ys = x `elem` sortFn (xs ++ [x] ++ ys)

--3.correct sorted?
sortProp3 :: ([Int] -> [Int]) -> [Int] -> Bool
sortProp3 sortFn xs = isSorted (sortFn xs)
  where 
    isSorted (x1 : x2 : xs) = (x1 <= x2) && isSorted (x2 : xs)
    isSorted _ = True

--4.length stay the same after sortFn
sortProp4 :: ([Int] -> [Int]) -> [Int] -> Bool    
sortProp4 sortFn xs = length xs == length (sortFn xs)

--5.use a built-in sort to compare my output with expected output
sortProp5 :: ([Int] -> [Int]) -> [Int] -> Bool
sortProp5 sortFn xs 
  = sortFn xs == insertionSort xs

insertionSort :: [Int] -> [Int]
insertionSort xs = foldr insertSorted [] xs
  where 
    insertSorted x [] = [x]
    insertSorted x (y : ys) 
      | x <= y = x : y : ys
      | otherwise = y : insertSorted x ys

--lookupcouts