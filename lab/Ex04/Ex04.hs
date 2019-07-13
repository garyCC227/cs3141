module Ex04 where

import Text.Read (readMaybe)

data Token = Number Int | Operator (Int -> Int -> Int) 

parseToken :: String -> Maybe Token
parseToken "+" = Just (Operator (+))
parseToken "-" = Just (Operator (-))
parseToken "/" = Just (Operator div)
parseToken "*" = Just (Operator (*))
parseToken str = fmap Number (readMaybe str)

tokenise :: String -> Maybe [Token]
tokenise str = mapM parseToken ( (reverse.words) str)


newtype Calc a = C ([Int] -> Maybe ([Int], a))


pop :: Calc Int
pop = C $ \xs ->
  case xs of
    [] -> Nothing
    x:xs -> Just (xs, x)

-- pop = C f
--   where
--     f :: [Int] -> Maybe ([Int], Int)
--     f [] = Nothing
--     f (x:xs) = Just (xs, x) 

push :: Int -> Calc ()
push i = C ( \xs -> Just (i:xs, ()) ) 

instance Functor Calc where
  fmap f (C sa) = C $ \s ->
      case sa s of 
        Nothing      -> Nothing
        Just (s', a) -> Just (s', f a)

{-
   -- f = (+ 3) or others, sf = (+)
-}
instance Applicative Calc where
  pure x = C (\s -> Just (s,x))
  C sf <*> C sx = C $ \s -> 
      case sf s of 
          Nothing     -> Nothing
          Just (s',f) -> case sx s' of
              Nothing      -> Nothing
              Just (s'',x) -> Just (s'', f x)

instance Monad Calc where
  return = pure
  C sa >>= f = C $ \s -> 
      case sa s of 
          Nothing     -> Nothing
          Just (s',a) -> unwrapCalc (f a) s'
    where unwrapCalc (C a) = a

-- x :: Token = Number Int | operator
-- Calc Int :: C (\xs -> (xs, Int))
evaluate :: [Token] -> Calc Int
-- evaluate [] = pop
-- evaluate (Number i: ts) = 
--   do
--     push i
--     evaluate ts
-- evaluate (t:ts) = 
--   do
--     evaluate ts
evaluate ts = f [] False ts
  where
    f:: [(Int -> Int -> Int)] -> Bool -> [Token] -> Calc Int
    f ops isPending [] =
        case ops of
          [] ->
            do
              pop
          (op:ops') ->
            do
              operand_1 <- pop
              operand_2 <- pop
              push (operand_1 `op` operand_2) -- TODO
              f ops' isPending []

    f ops isPending ((Number i):ts) = 
      do
        case ops of 
          []        -> do 
            -- if isPending == True, then we dont have a operator, then return Nothing
              C $ (\xs -> Nothing)
          (op:ops') ->
            do
              if isPending then do
                operand <- pop
                push (i `op` operand)
                f ops' isPending ts
              else 
                do
                  push i
                  f (op:ops') True ts

    f ops isPending ((Operator op): ts) = 
      do
        (f (pushOP op ops) False ts)

-- pushOp :: 
pushOP op [] =[op]
pushOP op xs = op:xs


calculate :: String -> Maybe Int
calculate s = f (tokenise s)
  where
    f :: Maybe [Token] -> Maybe Int
    f Nothing = Nothing
    f (Just ts) = helper (evaluate ts)
      where
        helper :: Calc Int -> Maybe Int
        helper (C intF) = helper2 (intF [])
          where helper2 (Just (xs, result)) = Just result
                helper2 Nothing = Nothing

calcHelp :: [Token] -> Calc Int
calcHelp ts = f [] False ts
  where
    f:: [(Int -> Int -> Int)] -> Bool -> [Token] -> Calc Int
    f ops isPending [] =
        case ops of
          [] ->
            do
              result <- pop
              pure result
          (op:ops') ->
            do
              operand_1 <- pop
              operand_2 <- pop
              push (operand_1 `op` operand_2) -- TODO
              f ops' isPending []

    f ops isPending ((Number i):ts) = 
      do
        case ops of 
          []        -> do 
            -- if isPending == True, then we dont have a operator, then return Nothing
              C $ (\xs -> Nothing)
          (op:ops') ->
            do
              if isPending then do
                operand <- pop
                push (i `op` operand)
                f ops' isPending ts
              else 
                do
                  push i
                  f (op:ops') True ts

    f ops isPending ((Operator op): ts) = 
      do
        (f (pushOP op ops) False ts)

-- -- pushOp :: 
-- pushOP op [] =[op]
-- pushOP op xs = op:xs