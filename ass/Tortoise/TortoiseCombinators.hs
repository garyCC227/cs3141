module TortoiseCombinators
       ( andThen 
       , loop 
       , invisibly 
       , retrace 
       , overlay 
       , lookUplc --TODO
       ,checkCList
       ,checkLList
       ,checkPList
       ) where

import Tortoise

-- See Tests.hs or the assignment spec for specifications for each
-- of these combinators.

andThen :: Instructions -> Instructions -> Instructions
andThen Stop i2 = i2
andThen i1 Stop = i1
andThen (PenDown i) i2 = PenDown $ andThen i i2
andThen (PenUp i) i2 = PenUp $ andThen i i2
andThen (Turn r i) i2 = Turn r $ andThen i i2
andThen (SetStyle l i) i2 = SetStyle l $ andThen i i2
andThen (SetColour c i) i2 = SetColour c $ andThen i i2
andThen (Move m i) i2 = Move m $ andThen i i2

-- 0 and negative number will do nothing
loop :: Int -> Instructions -> Instructions
loop n i
    | n <= 0 = Stop
    | n == 1 = i
    | otherwise = 
        i `andThen` (loop (n-1) i) 

--si == sub instructions
--cout == 0, penDown == False
--cout == 1, penDown == True
invisibly :: Instructions -> Instructions
invisibly i = PenUp $ coutUpDown i 1
    where
        coutUpDown :: Instructions -> Int -> Instructions
        coutUpDown (PenDown si) cout = coutUpDown si 1
        coutUpDown (PenUp si) cout = coutUpDown si 0
        coutUpDown (Turn r si) cout = Turn r $ coutUpDown si cout
        coutUpDown (Move m si) cout = Move m $ coutUpDown si cout
        coutUpDown (SetStyle l si) cout = SetStyle l $ coutUpDown si cout
        coutUpDown (SetColour c si) cout = SetColour c $ coutUpDown si cout
        coutUpDown Stop cout 
            | cout == 1 = PenDown $ Stop
            | otherwise = Stop

--------For retrace
data PenState = Down | Up
        deriving (Show, Eq)

{-  
    a function for look up line , color, penup or down
    store in backward order
-}
lookUplc :: Instructions -> ([Colour], [LineStyle], [PenState])
lookUplc Stop = ([], [] , [])
lookUplc (PenDown i) = let (cs, ls, ps) = lookUplc i
                        in (cs, ls, (Down:ps) )
lookUplc (PenUp i) = let (cs, ls, ps) = lookUplc i
                        in (cs, ls, (Up:ps) )

lookUplc (Turn r i) = let (cs, ls, ps) = lookUplc i
                        in (cs, ls, ps)

lookUplc (Move m i) = let (cs, ls, ps) = lookUplc i
                        in (cs, ls, ps)

lookUplc (SetStyle l i) = let (cs, ls, ps) = lookUplc i
                            in (cs, (l:ls) , ps )

lookUplc (SetColour c i) = let (cs, ls, ps) = lookUplc i
                        in ( (c:cs), ls, ps)

takeFirst :: ([Colour], [LineStyle], [PenState]) -> [Colour]
takeFirst (cs, ls, ps) = cs

takeSec :: ([Colour], [LineStyle], [PenState]) -> [LineStyle]
takeSec (cs, ls, ps) = ls

takeThird :: ([Colour], [LineStyle], [PenState]) -> [PenState]
takeThird (cs, ls, ps) = ps

{--
 - if more than 1 elements in the list, add default in the front
    size = 1 + how many instruction of specific action
-}
checkCList :: [Colour] -> Colour-> [Colour]
checkCList css initC = 
    case css of
        [] -> []
        [c] -> [c]
        (c:cs) -> initC:c:cs

checkLList :: [LineStyle] -> LineStyle-> [LineStyle]
checkLList lss initL = 
    case lss of
        [] -> []
        [l] -> [l]
        (l:ls) -> initL:l:ls

checkPList :: [PenState] -> PenState-> [PenState]
checkPList pss initP = 
    case pss of
        [] -> []
        [p] -> [p]
        (p:ps) -> initP:p:ps

{-
Time complexity of this function:
    n for lookUplc
    n for retraceHelp
    so O(2n) = O(n)
-}
retrace :: Instructions -> Instructions
retrace initi = retraceHelp initi Stop cs ls ps
    where
        -- look up all the color, line , pen state in O(n)
        results :: ([Colour], [LineStyle], [PenState])
        results = lookUplc initi
        
        -- so all stored in forward order
        -- for edge case: empty list, so we will have handle it   
        cs = checkCList  (takeFirst results) white
        ls = checkLList (takeSec results) (Solid 1)
        ps = checkPList (takeThird results) Down

        retraceHelp :: Instructions -> Instructions
                        -> [Colour] -> [LineStyle] -> [PenState]
                        -> Instructions
        retraceHelp (Move m i) is cs ls ps = retraceHelp i (Move (-m) $ is) cs ls ps
        retraceHelp (Turn r i) is cs ls ps = retraceHelp i (Turn (-r) $ is) cs ls ps
        retraceHelp Stop is cs ls ps = is
        --setLineStyle pattern
        -- if ls is empty, set to default line -> Solid 1
        retraceHelp (SetStyle l i) is cs ls ps = 
            case ls of 
                [l] -> retraceHelp i ( SetStyle (Solid 1) $ is) cs ls ps
                (l:lss) -> retraceHelp i ( SetStyle l $ is) cs lss ps
        
        -- default is white
        retraceHelp (SetColour c i) is cs ls ps = 
            case cs of 
                [c] -> retraceHelp i ( SetColour (white) $ is) cs ls ps
                (c:css) -> retraceHelp i ( SetColour c $ is) css ls ps
        
        -- -- default pen is true
        retraceHelp (PenDown i) is cs ls ps = 
            case ps of 
                [p] -> retraceHelp i ( PenDown $ is) cs ls ps
                (Down:pss) -> retraceHelp i ( PenDown $ is) cs ls pss
                (Up:pss) -> retraceHelp i (PenUp $ is) cs ls pss
        
        retraceHelp (PenUp i) is cs ls ps = 
            case ps of 
                [p] -> retraceHelp i ( PenDown $ is) cs ls ps
                (Down:pss) -> retraceHelp i ( PenDown $ is) cs ls pss
                (Up:pss) -> retraceHelp i (PenUp $ is) cs ls pss

overlay :: [Instructions] -> Instructions
overlay is = error "'overlay' unimplemented"




