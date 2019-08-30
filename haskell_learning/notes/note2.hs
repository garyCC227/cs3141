-- Function
--
-- -** Normal function
-- add :: Integer -> Integer -> Integer   --function declaration
-- add x y =  x + y                       --function definition
--
-- -** Pattern matching(like in prolog)
-- -- has base case or recursive case
-- fact :: Int -> Int
-- fact 0 = 1
-- fact n = n * fact ( n - 1 )
--
-- main = do
--    putStrLn "The factorial of 5 is:"
--    print (fact 5)
--
--
-- -** Guard(like switch case, do all the cases in one function)
-- -- by using the oeprator " | "
-- fact :: Integer -> Integer
-- fact n | n == 0 = 1
--        | n /= 0 = n * fact (n-1)
-- main = do
--    putStrLn "The factorial of 5 is:"
--    print (fact 5)
--
-- -** where : using for expression with multiple variables.
-- roots :: (Float, Float, Float) -> (Float, Float)
-- roots (a,b,c) = (x1, x2) where
--    x1 = e + sqrt d / (2 * a)
--    x2 = e - sqrt d / (2 * a)
--    d = b * b - 4 * a * c
--    e = - b / (2 * a)
-- main = do
--    putStrLn "The roots of our Polynomial equation are:"
--    print (roots(1,-8,6))
--
-- -** higher order function( take function as argument)
--     import Data.Char
--     import Prelude hiding (map)
--
--     map :: (a -> b) -> [a] -> [b]
--     map _ [] = []
--     map func (x : abc) = func x : map func abc
--     main = print $ map toUpper "tutorialspoint.com"

-- -** lamda function, using "\"
-- main = do
--    putStrLn "The successor of 4 is:"
--    print ((\x -> x + 1) 4)   -- (\x -> x + 1) is a function, 4 is argument
or -
    \x y -> x+y

-** function composition "." -- function compostion operator

main = do
   putStrLn "Example of Haskell Function composition"
   print ((noto.eveno)(16))
   -- function eveno take argument 16, noto take argument eveno(16)
   -- you know it, like composition in COMP2111

-aaa x = case x of
            []  -> [1]
            [x] -> [x]
            (x:xs) -> xs

- function implementation.
    -ps s = unwords $ map p (words s)
        where
            p s = "(" ++ s ++ ")"
    -ps s = unwords $ map (\s -> "(" ++ s ++")") (words s)
    -p = \s -> "(" ++ s ++")"
    main = do
        print(p "hallodsadas")
