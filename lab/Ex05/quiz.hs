{-# LANGUAGE FlexibleContexts #-}
module Ex05 where
import Text.Read (readMaybe)
import System.IO
import Data.Char
import System.Environment
import Control.Monad.State
import System.Random
import Test.QuickCheck
import Data.Maybe

a = do x <- getLine
       putStrLn (filter isDigit x)
       a 
a1 = getLine >>= putStrLn . filter isDigit >> a1
a2 = getLine >>= \x -> putStrLn (filter isDigit x) >>= \_ -> a2
-- a3 = (getLine >>= \x -> putStrLn (filter isDigit x)) >>= a3
-- a5 = do x <- getLine; putStrLn . filter isDigit; a5
-- a4 = do getLine >>= \x -> putStrLn . filter isDigit; a4
a6 = do x <- getLine; putStrLn . filter isDigit $ x; a6
a7 = do getLine >>= \x -> putStrLn (filter isDigit x); a7

-- leftPad :: Int -> State String ()
-- leftPad l = while ((< l) . length) $ do
--               str <- get
--               put (' ':str)

matching :: String -> Int -> Bool
matching []       n = (n == 0)
matching ('(':xs) n = matching xs (n+1)
matching (')':xs) n = n > 0 && matching xs (n-1)
matching (oth:xs) n = matching xs n

-- matching xs = snd (runState (go xs) 0) == 0
--   where
--     go [] = pure True
--     go (x:xs) | x == '('  = modify (+1) >> go xs
--               | x == ')'  = modify (-1) >> go xs
--               | otherwise = go xs 

-- matching' xs = let (b,n) = runState (go xs) 0
--                in b && (n == 0)
--   where
--     go [] = pure True
--     go (x:xs) | x == '('  = modify (+1) >> go xs
--               | x == ')'  = modify (-1) >> go xs
--               | otherwise = go xs 
matching' xs = fst (runState (go xs) 0)
  where
    go [] = get >>= pure . (== 0)
    go (x:xs) | x == '('  = modify (+1) >> go xs
              | x == ')'  = do n <- get
                               if n > 0 then put (n - 1) >> go xs
                                        else pure False
              | otherwise = go xs 
-- matching' xs = fst (runState (go xs) 0)
--   where
--     go [] = pure True
--     go (x:xs) | x == '('  = modify (+1) >> go xs
--               | x == ')'  = do n <- get
--                                if n > 0 then put (n - 1) >> go xs
--                                         else pure False
--               | otherwise = go xs 
