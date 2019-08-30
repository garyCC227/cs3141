main = putStrLn (greet "gary")

greet who = "hello, " ++ who
---------
add :: Int -> Int -> Int
add a b =  a + b

--------
x = [(1, "one"), (2,"two")]
temp = print( lookup 1 x)

--- create our data type
data Compass = East | South| West | North
    deriving (Eq, Ord, Enum, Show)
--- create our own instance
-- instance Eq Compass where
--     show North = "North"

data Expression = Number Int
                | Add Expression Expression
                | Subtract Expression Expression
                deriving(Eq, Ord, Show)


-- pattern matching
calculate :: Expression -> Int
calculate (Number x ) = x
calculate (Add x y) = (calculate x) + (calculate y)
calculate (Subtract x y) = (calculate x) - (calculate y) -- some errors

---pattern matching in list
newHead :: [a] -> a
newHead [] = error "empty list"
newHead (a:ax) = a

newTail :: [a] ->[a]
newTail [] = error "empty list"
newTail (a:ax) = ax
