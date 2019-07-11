import Test.QuickCheck

-------------
newtype AlphaString = 
    AlphaString [Char] 
      deriving (Show)
  
instance Arbitrary AlphaString where
    arbitrary = AlphaString <$> (listOf $ elements "ABCDEFGHYJKLMNOPQRSTUVWXYZ")
  