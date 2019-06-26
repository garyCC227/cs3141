module Main where
import Tortoise
import TortoiseCombinators
import TestSupport (MoveTurnOnly(..), NoPenControl(..))

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Data.List(reverse)

-----------------------start of andThen
prop_andThen_left_id  i = 

    Stop `andThen` i == i

prop_andThen_right_id i = 

    i `andThen` Stop == i

prop_andThen_assoc i1 i2 i3 = 

    (i1 `andThen` i2) `andThen` i3 == i1 `andThen` (i2 `andThen` i3)

prop_andThen_compose i1 i2 = 

    tortoise (i1 `andThen` i2) start == comp (tortoise i1) (tortoise i2) start

-------------------------------end of andThen
prop_loop_compose n i = 

    tortoise (loop n i) start == foldr comp nop (replicate n (tortoise i)) start

-------------------------------end of loop

prop_invisibly_sems i = 

    tortoise (invisibly i) start == ([], finalState i)

--------------------------------end of invisibily

prop_retrace_sems i = 

    tortoise (retrace i) (finalState i) == (reverse (tortoisePic i), start)

--------------------------------end of retrace
prop_overlay_finalState is = 

    finalState (overlay is) == start

prop_overlay_concat is = 

    tortoisePic (overlay is) == concatMap tortoisePic is

------------------------end of overlay

tests = testGroup "tests" [ 
      testGroup "andThen" [
          testGroup "Monoid Laws" [
              testProperty "left identity"  prop_andThen_left_id,
              testProperty "right identity" prop_andThen_right_id,
              testProperty "associativity"  prop_andThen_assoc
          ],
          testProperty "semantics" prop_andThen_compose
      ],
      testGroup "loop" [
         testProperty "n-ary composition" prop_loop_compose
      ],
      testGroup "invisibly" [
         testProperty "semantics" prop_invisibly_sems
      ],
      testGroup "retrace" [
         testProperty "semantics" prop_retrace_sems
      ],
      testGroup "overlay" [
         testProperty "final state" prop_overlay_finalState,
         testProperty "concat" prop_overlay_concat
      ]
  ]         
                         
main = defaultMain tests


----- own things
-- lhs n i= tortoise (loop n i) start
-- rhs n i= foldr comp nop (replicate n (tortoise i)) start

-- action = Turn 49 Stop
lhs i = tortoise (retrace i) (finalState i) 
rhs i = (reverse (tortoisePic i), start)

input = SetColour (Colour {redC = 0, greenC = 0, blueC = 255, alphaC = 255}) (SetColour (Colour {redC = 0, greenC = 0, blueC = 0, alphaC = 255}) Stop)
