

-- * custome data type synonyms
using type keyword.
    type Point = (Float, Float)

    -- * create own custome data type
using 'data' keyword

data Point = Point Float Float
      name,  constructor, constructor arguments

      --constructor can have diff name to type name.

-- paramaetric types with recursive definition
data Tree a = Empty | branch a (Tree a) (Tree b)
