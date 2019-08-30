-- -- Haskell - a purely functional programming language(FP).
-- in imperative languages is about giving state to computor to execute.
-- but purely functional programming is tell computor the model, but dont tell what to do(enviroment)


-- everything in FP is a function;
-- avoid looping instead using recursion


-- :t , is to specify the type of input
-- :: , has type operator
-- _ , like prolog, things we dont care
-- : , add as head operator,
-- ++ two list add together
-data type
 -- number: same as other language
 -- char :'', string:  "".
 -- boolean True && False
 -- list: [],  not allow diff type
    -- **list comprehension.
        -- [output| range, condition]
        -- [x*2| x<-[1..10]], list is x*2, and assign x to [1..10]
    -- **list built-in function( argument is a list, and doesnt change the original list )
        - head
        - tail -- [H|tail] so print out tail, u know it
        - last
        - init -- list without the last element
        - null -- check if empty or not
        - reverse
        - length
        work on string as well - take 5 ([1..10]) -- take the first 5 element to generate a new sub-list, also work on string
        work on string as well- drop 5 ([1..10]) -- drop the first 5 element and use the remaining to generate a new sub-list, also work on string
        - maximum
        - minimum
        - sum
        - product
        - elem 99 (x), check if 99 in list x
            main = do
               let x = [1,45,155,1785]
               putStrLn "Our list is:"
               print (x)
               putStrLn "Does it contain 786?"
               print (elem 786 (x))
 -- tuple: (x,y). immutable. allow diff type
 -- data type: Maybe a = Nothing | just a
type class
 -- Int 2147483647 to - 2147483647
 -- Integer: no bounded
 -- float
 -- double
 --bool

-operator
+, - , *, /
-- sequence or range operator: ..
[1..10]

-- if statement,
 if condi
     then
else if condi
     then
else

aids function
- show [1..10],   print its argument as a string
- read "[1..10]", read the arugument,  then to do something else
- 2::Double, convert to double
putStrLn -- put a string with a new line



Function
- add:: Int -> Int -> Int  -- function declaration
- add x y = x + y          -- function definition
