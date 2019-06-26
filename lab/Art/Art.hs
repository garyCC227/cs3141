module Art where  

import ShapeGraphics
import Codec.Picture

art :: Picture
art = myArt 10 500 100 ++ (myCir 100 50)
   

fracTree :: Float -> Float -> Int -> Picture
fracTree width height n
  = fTree (Point  (400 - width / 2) 800) (Vector 0 (-height))
                  (Vector width 0) red n
  where
    
    toBlue :: Colour -> Colour
    toBlue (Colour r g b o) = 
      Colour (max 0 (r - 15)) g (min 255 (b + 15)) o
    angle = pi/8
    fTree :: Point -> Vector -> Vector -> Colour -> Int -> Picture
    fTree pos vec1 vec2 col n
      | n <= 1 = [Polygon [pos, movePoint vec1 pos, 
                                movePoint vec2 $ movePoint vec1 pos]
                          col
                          Solid
                          SolidFill]
                          
      | otherwise = fTree pos vec1 vec2 col (n - 1) ++
                    fTree (movePoint vec1 pos) 
                          (scaleVector 0.8 $ rotateVector (1.2 * angle) vec1)
                          (scaleVector 0.8 $ rotateVector (1.2 * angle) vec2) 
                          (toBlue col) (n - 1) ++
                    fTree (movePoint vec1 pos) 
                          (scaleVector 0.8 $ rotateVector (-angle) vec1)
                          (scaleVector 0.8 $ rotateVector (-angle) vec2) 
                          (toBlue col) (n - 1) 

      
scaleVector :: Float -> Vector -> Vector
scaleVector fac (Vector x y)
  = Vector (fac * x) (fac * y)                           
 
rotateVector :: Float -> Vector -> Vector
rotateVector alpha (Vector vx vy)
  = Vector (cos alpha * vx - sin alpha * vy)
           (sin alpha * vx + cos alpha * vy)

movePoint :: Vector -> Point -> Point
movePoint (Vector xv yv) (Point xp yp)
  = Point (xv + xp) (yv + yp)

-- use 'writeToFile' to write a picture to file "ex01.png" to test your
-- program if you are not using Haskell for Mac
-- e.g., call
-- writeToFile [house, door]

writeToFile n
  = writePng "art.png" (drawPicture 3 art)


  {- my own image -}

colorWheel :: Float -> Float -> Colour
colorWheel x opa = Colour r g b a
  where
    r = floor $ (cos x + 1) * (255 / 2)
    g = floor $ (sin x + 1) * (255 / 2)
    b = floor $ (cos (x+(pi/2)) + 1) * (255 / 2)
    a = max 50 (floor $ (cos (x+(pi/2)) + 1) * (opa))

steps :: (Floating b, Enum b) => b -> [b]
steps n = map (\i -> i * (2*pi/n)) [0 .. n]

myPic :: Point -> Float -> Float -> Float -> PictureObject
myPic (Point x y) r theta opa = Circle pos2 (r/10 * 5) col Solid NoFill
  where 
    pos2 :: Point
    pos2 = pairToPoint ((r * cos theta) + x,(r * sin theta) + y)

    col :: Colour
    col = colorWheel theta opa


myArt :: Float -> Float -> Float -> Picture
myArt n r opa = 
  rotateTri (Point 400 400) n r opa 
    where
      rotateTri :: Point -> Float -> Float -> Float -> Picture
      rotateTri pos n radius opa
        | radius <= 200 = [ (myPic pos radius theta opa) | theta <- steps n]
        | otherwise = [ (myPic pos radius theta opa) | theta <- steps n] ++
                      rotateTri pos n (radius - 10) (opa+5)

myCir :: Float -> Int -> Picture
myCir r o = repeat r o 
  where 
    repeat :: Float -> Int -> Picture
    repeat r o
      | r <= 5 = [ (Circle (Point 400 400) r myYellow{opacityC = min 50 (180 - o)} Solid SolidFill) ]
      | otherwise = [(Circle (Point 400 400) r myYellow{opacityC= min 50 (180 - o)} Solid SolidFill)] ++
                  repeat (r-10) (o+20)

myYellow :: Colour
myYellow = Colour 248 222 189 255

pairToPoint :: (Float, Float) -> Point
pairToPoint (x,y) = Point x y


--(fracTri 100 100 20)
-- (myElp 200 100 15 50)

-- tri = Polygon pos myCol Solid SolidFill
--   where pos = map pairToPoint snow1COs


-- --own things

-- pos = Point 400 400

-- myCol = Colour 200 66 244 150
-- myYellow = Colour 248 222 189 255


-- snow1COs = [(300,350), (350,250), (250,250)]
-- snow2COs = [(275,250), (325,250), (300,300)]

-- myPic2 (Point x y) col n = [Circle pos (n*5) (toOpac myYellow) Solid SolidFill,
--                           Circle pos (n*5) myCol Solid SolidFill
--                           ]

-- toOpac (Colour r g b o) = 
--   Colour r g b (max 30 (o-30))


-- myRed n 
--   | n `mod` 2 /= 0 = red { opacityC = max 0 (255-5*n)}
--   | otherwise = toBlue red n
--     where
--       toBlue :: Colour -> Int -> Colour
--       toBlue (Colour r g b o) n = 
--         Colour (max 0 (r - 15)) g (min 255 (b + 15)) ( o+5*n)

-- myElp weight rotate n opacity
--   | n <= 1 = [Ellipse (Point 400 400) weight (50+5*n) rotate (myRed opacity) Solid NoFill]
--   | otherwise = myElp (max 20 (weight-5*n)) (rotate*n) (n-1) (opacity+1)++
--                 myElp (weight+2*n) (rotate*(n-1)) (n-2) (opacity+2)