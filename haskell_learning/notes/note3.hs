-** modules(google yi xia)
    - set
    - list
    - Map
    - char
    - custom
        module Custom(
            showeven,
            love
        )where
            showeven :: --declaration
            showeven = ... --definition

-** one of the build-in haskell files and stream function
    - readFile file
    - writeFile file "input string"
    let file = "abc.txt"

-** exception.
    - import Control.Exception
    import Control.Exception

main = do
   result <- try (evaluate (5 `div` 0)) :: IO (Either SomeException Int)
   case result of
      Left ex   -> putStrLn $ "Caught exception: " ++ show ex
      Right val -> putStrLn $ "The answer was: " ++ show val


-** functor
  fmap -- map a function to another function as output
  functor allow us to implement so many data type where map cannot do
      - class Functor f where
          fmap :: (a -> b) -> f a -> f b
      - like fmap (+7)(Just 10)
