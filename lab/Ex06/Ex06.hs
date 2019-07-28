{-# LANGUAGE GADTs #-}

module Ex06 where

import Data.List
-- Datatype of formulas
-- --------------------

data Formula ts where
  Body   :: Term Bool                     -> Formula ()
  Exists :: Show a 
         => [a] -> (Term a -> Formula as) -> Formula (a, as)

data Term t where
  Name    :: String -> Term t    -- to facilitate pretty printing only. 
                                 -- don't use this in actual formulae.

  Con     :: t -> Term t -- Constant values

  -- Logical operators
  And     :: Term Bool -> Term Bool -> Term Bool
  Or      :: Term Bool -> Term Bool -> Term Bool

  -- Comparison operators
  Smaller :: Term Int  -> Term Int  -> Term Bool

  -- Arithmetic operators
  Plus    :: Term Int  -> Term Int  -> Term Int


-- Pretty printing formulas
-- ------------------------

instance Show t => Show (Term t) where
  show (Con v)       = show v
  show (And p q)     = "(" ++ show p ++ " && " ++ show q ++ ")"
  show (Or p q)      = "(" ++ show p ++ " || " ++ show q ++ ")"
  show (Smaller n m) = "(" ++ show n ++ " < "  ++ show m ++ ")"
  show (Plus n m)    = "(" ++ show n ++ " + "  ++ show m ++ ")"
  show (Name name)   = name

instance Show (Formula ts) where
  show = show' ['x' : show i | i <- [0..]]
    where
      show' :: [String] -> Formula ts' -> String
      show' ns     (Body body)   = show body
      show' (n:ns) (Exists vs p) = "exists " ++ n ++ "::" ++ show vs ++ ". " ++ show' ns (p (Name n))


-- Example formulas
-- ----------------

ex1 :: Formula ()
ex1 = Body (Con True)

ex2 :: Formula (Int, ())
ex2 = Exists [1..10] $ \n ->
        Body $ n `Smaller` (n `Plus` Con 1)

ex3 :: Formula (Bool, (Int, ()))
ex3 = Exists [False, True] $ \p -> 
      Exists [0..2] $ \n -> 
        Body $ p `Or` (Con 0 `Smaller` n)

-- ex3 :: Formula (Bool, (Int, ()))
ex5 = Exists [False] $ \p -> 
      Exists [0] $ \n -> 
        Body $ p `Or` (Con 0 `Smaller` n)

-- ex3 :: Formula (Bool, (Int, ()))
ex4 = Exists [False, True] $ \p -> 
      Exists [0..2] $ \n -> 
      Exists [2..4] $ \m ->  
        Body $ p `Or` (Con 0 `Smaller` (n `Plus` m)  )

-- Evaluating terms
-- ----------------
eval :: Term t -> t
eval (Con v) = v
eval (And p q) = (eval p && eval q)
eval (Or p q) = (eval p || eval q)
eval (Smaller i1 i2) = (eval i1 < eval i2)
eval (Plus i1 i2) = eval i1 + eval i2  


-- the Name constructor is not relevant for evaluation
-- just throw an error if it is encountered:
eval (Name _) = error "eval: Name"   


-- Checking formulas
-- -----------------

satisfiable :: Formula ts -> Bool
satisfiable (Body t) = eval t
satisfiable (Exists [] f) = False
satisfiable (Exists (t:ts) f) = satisfiable (f (Con t) ) ||
                                satisfiable (Exists ts f)


-- Enumerating solutions of formulae
-- ---------------------------------

solutions :: Formula ts -> [ts]
solutions ts = correctSolution (solutions' ts) (solutionList ts) []
  where
    correctSolution :: [ts] -> [Bool] -> [ts] -> [ts]
    correctSolution [] [] result = reverse result
    correctSolution (x:xs) (b:bs) result = 
      case b of
        False -> correctSolution xs bs result
        True -> correctSolution xs bs (x:result)


solutions' :: Formula ts -> [ts]
solutions' (Body t) = [()]
solutions' (Exists [] f) = []
solutions' (Exists (t:ts) f) = (format t (f (Con t))) ++ (solutions' (Exists ts f)) 
      

format :: a -> Formula ts -> [(a, ts)]
format t (Body body) = [(t, ())]
format t (Exists [] p) = []
format t (Exists (v:vs) p) = do
  let fml = p (Con v);
      ts = foldr ((++) . (\x -> format x fml)) [] (v:vs)
  map ((,) t) ts

solutionList :: Formula ts -> [Bool]
solutionList (Body t) = [eval t]
solutionList (Exists [] f) = []
solutionList (Exists (t:ts) f) = 
  solutionList (f (Con t)) ++ solutionList (Exists ts f)


prop_length ts = (length . solutions') ts == ((length . solutionList) ts)  