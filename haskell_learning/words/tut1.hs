import Data.Char
test = do
    let increment x = 1 + x
        in \xs -> map chr (map increment (map ord xs))
