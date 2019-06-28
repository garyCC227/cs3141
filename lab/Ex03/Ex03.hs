module Ex03 where

import Test.QuickCheck
import Data.List(sort, nub, delete)

data BinaryTree = Branch Integer BinaryTree BinaryTree
                | Leaf
                deriving (Show, Ord, Eq)

isBST :: BinaryTree -> Bool
isBST Leaf = True
isBST (Branch v l r) 
  = allTree (< v) l  && 
    allTree (>= v) r &&
    isBST l          && 
    isBST r
  where allTree :: (Integer -> Bool) -> BinaryTree -> Bool
        allTree f (Branch v l r) = f v && allTree f l && allTree f r
        allTree f (Leaf) = True
        
--Add an integer to a BinaryTree, preserving BST property.
insert :: Integer -> BinaryTree -> BinaryTree
insert i Leaf = Branch i Leaf Leaf
insert i (Branch v l r) 
  | i < v     = Branch v (insert i l) r
  | otherwise = Branch v l (insert i r)

--Remove all instances of an integer in a binary tree, preserving BST property
deleteAll :: Integer -> BinaryTree -> BinaryTree
deleteAll i Leaf = Leaf
deleteAll i (Branch j Leaf r) | i == j = deleteAll i r
deleteAll i (Branch j l Leaf) | i == j = deleteAll i l
deleteAll i (Branch j l r) | i == j = let (x, l') = deleteRightmost l
                                       in Branch x l' (deleteAll i r)
                           | i <  j = Branch j (deleteAll i l) r
                           | i >  j = Branch j l (deleteAll i r)
  where deleteRightmost :: BinaryTree -> (Integer, BinaryTree)
        deleteRightmost (Branch i l Leaf) = (i, l)
        deleteRightmost (Branch i l r)    = let (x, r') = deleteRightmost r
                                             in (x, Branch i l r')

searchTrees :: Gen BinaryTree
searchTrees = sized searchTrees'
  where 
   searchTrees' 0 = return Leaf
   searchTrees' n = do 
      v <- (arbitrary :: Gen Integer)
      fmap (insert v) (searchTrees' $ n - 1)

----------------------

-- function for `elem` in this BST ?
mysteryPred :: Integer -> BinaryTree -> Bool
mysteryPred i (Leaf) = False
-- leaf on right. only check if i == root
mysteryPred i (Branch v l Leaf) 
  | i == v = True
  | i < v = mysteryPred i l
  | otherwise = False
mysteryPred i (Branch v Leaf r)
  | i == v = True
  | i < v = False
  | otherwise = mysteryPred i r
mysteryPred i (Branch v l r)
  | i == v = True
  | i < v = mysteryPred i l
  | otherwise = 
            mysteryPred i r




prop_mysteryPred_1 integer = 
  forAll searchTrees $ \tree -> mysteryPred integer (insert integer tree)

prop_mysteryPred_2 integer = 
  forAll searchTrees $ \tree -> not (mysteryPred integer (deleteAll integer tree))

----------------------
-- to abstract function . which time complexity is bad :( so sad
mysterious :: BinaryTree -> [Integer]
mysterious t = toAbstract t
  where
    toAbstract :: BinaryTree -> [Integer]
    toAbstract (Leaf) = []
    toAbstract (Branch v Leaf r) =
      let rs = toAbstract r
        in (v : rs)
    toAbstract (Branch v l Leaf) = 
      let ls = toAbstract l
        in (ls ++ [v])
    toAbstract (Branch v l r) = 
      let ls = toAbstract l
          rs = toAbstract r
        in (ls ++ (v:rs) )
      


isSorted :: [Integer] -> Bool
isSorted (x:y:rest) = x <= y && isSorted (y:rest)
isSorted _ = True

prop_mysterious_1 integer = forAll searchTrees $ \tree -> 
  mysteryPred integer tree == (integer `elem` mysterious tree)

prop_mysterious_2 = forAll searchTrees $ isSorted . mysterious
----------------------

{-
  - right left case: 
      (1) right roate -> right child x:
          - successor is its left child lx',
          - then right child rx'' of lx' will be left child of x
      (2) left rotate the root :
        (1) its right child x will be succesor
        (2) left child of x -> lx will be right child of root


  for this function. to find the first unbalance branch
-}
-- Note `nub` is a function that removes duplicates from a sorted list
sortedListsWithoutDuplicates :: Gen [Integer]
sortedListsWithoutDuplicates = fmap (nub . sort) arbitrary

astonishing :: [Integer] -> BinaryTree
-- astonishing = undefined
astonishing [] = Leaf
astonishing xs = formVTL Leaf xs

formVTL :: BinaryTree -> [Integer] -> BinaryTree
formVTL tr [x] = insert x Leaf
formVTL tr xs = foldr insert Leaf $ (reverse' . preOrderList) xs

reverse' :: [Integer] -> [Integer]
reverse' l = go l []
  where go [] a = a
        go (x:xs) a = go xs (x:a)

preOrderList :: [Integer] -> [Integer]
preOrderList [] = []
preOrderList [x] = [x]
preOrderList xs = 
  let m = middleEl xs
      xs' = delete m xs
      --split and conquer algorithm
      (ls, rs) = split xs'
      ls' = preOrderList ls
      rs' = preOrderList rs
    in (m: (merge ls' rs') )

merge :: [Integer] -> [Integer] -> [Integer]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) = x:y:(merge xs ys)

-- Element in middle
middleEl :: [Integer] -> Integer
middleEl s = mEl s s 

mEl :: [Integer] ->  [Integer] -> Integer
mEl    []    (h:s2) = h
mEl (_:[])   (h:s2) = h
mEl (_:_:s1) (_:s2) = mEl s1 s2

startSplit :: [Integer] -> Int -> ([Integer], [Integer])
startSplit xs length = ((take length xs) , (drop length xs) )

--split list to half
split :: [Integer] -> ([Integer],[Integer])
split [] = ([],[])
split [x] = ([x],[]) 
split xs = 
  let len = length xs
  in
    if len `mod` 2 == 0 then
      startSplit xs (len `div` 2)
    else
      startSplit xs ((len `div` 2) + 1) 

prop_astonishing_1 
  = forAll sortedListsWithoutDuplicates $ isBST . astonishing

prop_astonishing_2 
  = forAll sortedListsWithoutDuplicates $ isBalanced . astonishing

prop_astonishing_3 
  = forAll sortedListsWithoutDuplicates $ \ integers -> 
    mysterious (astonishing integers) == integers


isBalanced :: BinaryTree -> Bool
isBalanced Leaf = True
isBalanced (Branch v l r) = and [ abs (height l - height r) <= 1 
                                , isBalanced l 
                                , isBalanced r
                                ]
  where height Leaf = 0
        height (Branch v l r) = 1 + max (height l) (height r)

